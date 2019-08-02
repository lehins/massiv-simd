{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- |
-- Module      : Data.Massiv.Array.ForeignArray
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.ForeignArray
  ( ForeignArray(..)
  , lengthForeignArray
  -- * Memory allocation
  , newForeignArray
  , mallocForeignArray
  , callocForeignArray
  , reallocForeignArray
  -- * Memory adjustment
  , copyForeignArray
  , moveForeignArray
  , fillForeignArray
  -- * Aligned memory allocation
  , newAlignedForeignArray
  , mallocAlignedForeignArray
  , callocAlignedForeignArray
  -- , reallocAlignedForeignArray
  , isSameForeignArray
  , withForeignArray
  , readForeignArray
  , writeForeignArray
  , sliceForeignArray
  , extractForeignArray
  -- ** Modifying
  , castForeignArray
  , setWithForeignArray
  , copyWithForeignArray
  , fillWithAlignedForeignArray
  , copyWithAlignedForeignArray
  -- ** Folding
  , eqWithForeignArray
  , foldWithForeignArray
  , fold2WithForeignArray
  , eqWithAlignedForeignArray
  , foldWithAlignedForeignArray
  , fold2WithAlignedForeignArray
  , foldNonEmptyWithAlignedForeignArray
  -- ** Lifting
  , liftForeignArray
  , zipWithForeignArray
  , liftAlignedForeignArray
  , liftAlignedForeignArray'
  , zipWithAlignedForeignArray
  -- ** Numeric
  , evenPowerSumAlignedForeignArray
  , absPowerSumAlignedForeignArray
  , multiplySumAlignedForeignArray
  ) where

import Control.DeepSeq
import Control.Monad.Primitive
import Data.Coerce
import Data.Massiv.Array.Unsafe (Sz(SafeSz))
import Data.Massiv.Core.Index
import Data.Primitive.ByteArray
import Data.Word
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Storable
import GHC.Exts
import GHC.ForeignPtr
import GHC.Ptr


#include "massiv.h"

data ForeignArray ix e = ForeignArray
  { foreignArraySize       :: !(Sz ix)
  -- ^ Size of the array. Can be less than the actual memory allocated
  --
  -- @since 0.1.0
  , foreignArrayFreeFlag   :: {-# UNPACK #-}!(Ptr CBool)
  -- ^ Whenever this pointer @false@ (i.e. contains @0@) the associated finalizer will
  -- free the memory during GC. Which means, if memory was freed by some other means (eg.
  -- as calling `free`) this pointer must be set to @true@ (i.e a non-zero value).
  --
  -- @since 0.1.0
  , foreignArrayPtr        :: {-# UNPACK #-}!(Ptr e)
  -- ^ Pointer to the beginning of the data, as far as this array is concerned, which
  -- means it does not necesserally points to the beginning of the allocated memory.
  --
  -- @since 0.1.0
  , foreignArrayForeignPtr :: !(ForeignPtr e)
  -- ^ Foreign pointer with a @free@ finalizer that is pointing to the beginning of
  -- allocated data. Finalizer will not attempt to free the memory if
  -- `foreignArrayFreeFlag` is set to @true@.
  --
  -- @since 0.1.0
  }


foreign import ccall unsafe "massiv.c &massiv_free_flagged"
  finalizerFreeFlagged :: FinalizerEnvPtr CBool a


instance NFData ix => NFData (ForeignArray ix e) where
  rnf (ForeignArray sz flag ptr (ForeignPtr _ c)) =
    sz `deepseq` flag `deepseq` ptr `deepseq` c `seq` ()



-- | Allocate pinned memory for the array on the GHC heap, but do not initialize any
-- elements.
--
-- @since 0.1.0
newForeignArray ::
     forall e ix. (Storable e, Index ix) => Sz ix -> IO (ForeignArray ix e)
newForeignArray sz = newAlignedForeignArray sz (alignment (undefined :: e))
{-# INLINE newForeignArray #-}


-- | Allocate an array using @malloc@, but do not initialize any elements.
--
-- @since 0.1.0
mallocForeignArray ::
     forall e ix. (Storable e, Index ix) => Sz ix -> IO (ForeignArray ix e)
mallocForeignArray = allocForeignArray mallocArray
{-# INLINE mallocForeignArray #-}


-- | Allocate an array using @calloc@, thus also initialize all elements to zero
--
-- @since 0.1.0
callocForeignArray ::
     (Storable e, Index ix) => Sz ix -> IO (ForeignArray ix e)
callocForeignArray = allocForeignArray callocArray
{-# INLINE callocForeignArray #-}


-- | Helper array allocator
allocForeignArray ::
     Index ix => (Int -> IO (Ptr e)) -> Sz ix -> IO (ForeignArray ix e)
allocForeignArray allocArray sz = do
  ptr <- allocArray (totalElem sz)
  freed <- calloc
  fptr <- newForeignPtrEnv finalizerFreeFlagged freed ptr
  pure $ ForeignArray sz freed ptr fptr
{-# INLINE allocForeignArray #-}


-- | Shrink or grow an array. If you need to check if the new array has been created or
-- the same one has been resized use `isSameForeignArray`
--
-- __Important__ - Growing might require a full copy to a new memory location, but
-- regardless if it was copied or not the new extra allocated area will not be
-- initialized. Most important is the old `ForeignArray` should not be used, unless
-- `isSameForeignArray` is `True` when appied to the argument and the result.
--
-- @since 0.1.0
reallocForeignArray ::
     forall ix' ix e . (Index ix', Index ix, Storable e)
  => ForeignArray ix' e
  -> Sz ix -- ^ New size. Can be bigger or smaller
  -> IO (ForeignArray ix e)
reallocForeignArray (ForeignArray sz freed curPtr fptr@(ForeignPtr _ c)) newsz
  | newLength == oldLength = pure (ForeignArray newsz freed curPtr fptr)
  | otherwise =
    withForeignPtr fptr $ \ptr ->
      let !offsetBytes@(I# offsetBytes#) = curPtr `minusPtr` ptr
          elementSize = sizeOf (undefined :: e)
          offset = offsetBytes `div` elementSize
          reallocMutableByteArray alloc mba#
            | newLength <= oldLength = do
              let !(I# newFullByteSize#) = newByteSize + offsetBytes
              primitive_ (shrinkMutableByteArray# mba# newFullByteSize#)
              pure (ForeignArray newsz freed curPtr fptr)
            | otherwise = do
              let !(I# oldByteSize#) = oldLength * elementSize
              fptr' <- alloc newByteSize (alignment (undefined :: e))
              -- We can't grow pinned memory in ghc, so we are left with manual create new
              -- and copy appraoch.
              withForeignPtr fptr' $ \ptr'@(Ptr addr#) -> do
                primitive_ (copyMutableByteArrayToAddr# mba# offsetBytes# addr# oldByteSize#)
                pure (ForeignArray newsz nullPtr ptr' fptr')
            where
              !newByteSize = newLength * elementSize
       in case c of
            PlainForeignPtr _ -> do
              rPtr <- reallocArray ptr (newLength + offset)
              if rPtr == ptr -- shrunk or grew without copy, so we can keep the same `fptr`
                then pure (ForeignArray newsz freed curPtr fptr)
                else do
                  poke freed 1
                  freed' <- calloc
                  fptr' <- newForeignPtrEnv finalizerFreeFlagged freed' rPtr
                  pure (ForeignArray newsz freed' (advancePtr rPtr offset) fptr')
            MallocPtr mba# _ -> reallocMutableByteArray mallocForeignPtrAlignedBytes mba#
            PlainPtr mba# -> reallocMutableByteArray mallocPlainForeignPtrAlignedBytes mba#
  where
    !oldLength = totalElem sz
    !newLength = totalElem newsz
{-# INLINE reallocForeignArray #-}

-- | Check if two foreign arrays refer to the same memory block.
--
-- @since 0.1.0
isSameForeignArray :: ForeignArray ix1 e1 -> ForeignArray ix2 e2 -> Bool
isSameForeignArray (ForeignArray _ _ _ (ForeignPtr p1 c1)) (ForeignArray _ _ _ (ForeignPtr p2 c2)) =
  case (c1, c2) of
    (PlainForeignPtr _, PlainForeignPtr _) -> Ptr p1 == Ptr p2
    (MallocPtr mba1# _, MallocPtr mba2# _) ->
      sameMutableByteArray (MutableByteArray mba1#) (MutableByteArray mba2#)
    (PlainPtr mba1#, PlainPtr mba2#) ->
      sameMutableByteArray (MutableByteArray mba1#) (MutableByteArray mba2#)
    _ -> False
{-# INLINE isSameForeignArray #-}

-- | Copy one array into another with @memcpy@. Copied areas may /not/ overlap.
copyForeignArray ::
     (Index ix2, Storable e)
  => ForeignArray ix1 e -- ^ Source array
  -> ForeignArray ix2 e -- ^ Destination array
  -> IO ()
copyForeignArray arrSrc arrDest =
  withForeignArray arrSrc $ \ ptrSrc ->
    withForeignArray arrDest $ \ ptrDest ->
      copyArray ptrDest ptrSrc (unSz (lengthForeignArray arrDest))
{-# INLINE copyForeignArray #-}

-- | Copy one array into another with @memmove@. Copied areas may overlap.
moveForeignArray ::
     (Index ix2, Storable e)
  => ForeignArray ix1 e -- ^ Source array
  -> ForeignArray ix2 e -- ^ Destination array
  -> IO ()
moveForeignArray arrSrc arrDest =
  withForeignArray arrSrc $ \ptrSrc ->
    withForeignArray arrDest $ \ptrDest ->
      moveArray ptrDest ptrSrc (unSz (lengthForeignArray arrDest))
{-# INLINE moveForeignArray #-}

-- | Fill an array with the same byte using @memset@.
fillForeignArray ::
     (Index ix, Storable e)
  => ForeignArray ix e -- ^ Source array
  -> Word8 -- ^ Byte value to use for filling the elements
  -> IO ()
fillForeignArray arr w8 =
  withForeignArray arr $ \ptr -> fillBytes ptr w8 (getsz arr undefined)
  where
    getsz :: Storable e => ForeignArray ix e -> e -> Int
    getsz _ dummy = unSz (lengthForeignArray arr) * sizeOf dummy
{-# INLINE fillForeignArray #-}


-------------
-- Aligned --
-------------

-- | Same as `newForeignArray`, but with ability to specify custom alignment.
--
-- @since 0.1.0
newAlignedForeignArray ::
     forall e ix. (Storable e, Index ix) => Sz ix -> Int -> IO (ForeignArray ix e)
newAlignedForeignArray sz align = do
  let dummy = undefined :: e
  fptr@(ForeignPtr ptr# _) <-
    mallocPlainForeignPtrAlignedBytes (totalElem sz * sizeOf dummy) align
  pure $ ForeignArray sz nullPtr (Ptr ptr#) fptr
{-# INLINE newAlignedForeignArray #-}


-- | Allocate an array, but do not initialize any elements.
--
-- @since 0.1.0
mallocAlignedForeignArray ::
  (Storable e, Index ix) => Sz ix -> Int -> IO (ForeignArray ix e)
mallocAlignedForeignArray = allocAlignedForeignArray mallocBytes
{-# INLINE mallocAlignedForeignArray #-}


-- | Allocate an array, but do not initialize any elements.
--
-- @since 0.1.0
callocAlignedForeignArray ::
  (Storable e, Index ix) => Sz ix -> Int -> IO (ForeignArray ix e)
callocAlignedForeignArray = allocAlignedForeignArray mallocBytes
{-# INLINE callocAlignedForeignArray #-}


allocAlignedForeignArray ::
     forall ix e. (Storable e, Index ix)
  => (Int -> IO (Ptr e))
  -> Sz ix
  -> Int
  -> IO (ForeignArray ix e)
allocAlignedForeignArray allocBytes sz align = do
  ptr <- allocBytes (sizeOf (undefined :: e) * totalElem sz + align - 1)
  freed <- calloc
  fptr <- newForeignPtrEnv finalizerFreeFlagged freed ptr
  pure $ ForeignArray sz freed (alignPtr ptr align) fptr
{-# INLINE allocAlignedForeignArray #-}


-- | Read en element from an array at a linear index. No bounds checking is performed.
--
-- @since 0.1.0
readForeignArray :: (Index ix, Storable e) => ForeignArray ix e -> Ix1 -> IO e
readForeignArray (ForeignArray sz _ curPtr fptr) =
  INDEX_CHECK("ForeignArray.readForeignArray",
              Sz . totalElem,
              \ _ i -> withForeignPtr fptr $ \_ -> peek (advancePtr curPtr i)) sz
{-# INLINE readForeignArray #-}

-- | Write an element to an array at a linear index. No bounds checking is performed.
--
-- @since 0.1.0
writeForeignArray :: (Index ix, Storable e) => ForeignArray ix e -> Ix1 -> e -> IO ()
writeForeignArray =
  INDEX_CHECK("ForeignArray.writeForeignArray",
              lengthForeignArray,
              \ arr i e -> withForeignArray arr $ \ptr -> poke (advancePtr ptr i) e)
{-# INLINE writeForeignArray #-}

-- | Take an outer slice of an array, thus lowering the dimensionality. Does no bounds
-- checking and no memory reallocation, just some pointer manipulations.
--
-- @since 0.1.0
sliceForeignArray ::
     forall ix e. (Index (Lower ix), Index ix, Storable e)
  => ForeignArray ix e
  -> Ix1 -- ^ Outer index the the slice should be taken at.
  -> ForeignArray (Lower ix) e
sliceForeignArray (ForeignArray sz freed curPtr fptr) i =
  ForeignArray (snd (unconsSz sz)) freed (advancePtr curPtr offset) fptr
  where
    offset = toLinearIndex sz (consDim i (zeroIndex :: Lower ix))
{-# INLINE sliceForeignArray #-}


-- | Adjust the size and move the pointer by an offset. Does no bounds checking and no
-- memory reallocation, just some pointer manipulations.
--
-- @since 0.1.0
extractForeignArray ::
     Storable e
  => Ix1 -- ^ Offset
  -> Sz ix -- ^ New size
  -> ForeignArray ix' e
  -> ForeignArray ix e
extractForeignArray i newsz (ForeignArray _ freed curPtr fptr) =
  ForeignArray newsz freed (advancePtr curPtr i) fptr
{-# INLINE extractForeignArray #-}

-- | Access the pointer to the beginning of this array.
--
-- @since 0.1.0
withForeignArray :: ForeignArray ix e -> (Ptr e -> IO a) -> IO a
withForeignArray (ForeignArray _ _ curPtr fptr) f =
  withForeignPtr fptr $ \_ -> f curPtr
{-# INLINE withForeignArray #-}



-- | Get the total number of elements in the array
--
-- @since 0.1.0
lengthForeignArray :: Index ix => ForeignArray ix e -> Sz1
lengthForeignArray (ForeignArray sz _ _ _) = SafeSz (totalElem sz)
{-# INLINE lengthForeignArray #-}


setWithForeignArray ::
     (Coercible a b, Storable a)
  => (Ptr b -> CLong -> b -> IO e)
  -> ForeignArray ix a
  -> Ix1
  -> Sz1
  -> a
  -> IO e
setWithForeignArray setAction arr offset sz e =
  withForeignArray arr $ \ptr ->
    setAction
      (coerce (advancePtr ptr offset))
      (fromIntegral (unSz sz))
      (coerce e)
{-# INLINE setWithForeignArray #-}


copyWithForeignArray ::
     (Storable a, Coercible a b)
  => (Ptr b -> Ptr b -> CLong -> IO ())
  -> ForeignArray ix1 a
  -> Ix1
  -> ForeignArray ix2 a
  -> Ix1
  -> Sz1
  -> IO ()
copyWithForeignArray copyWith arrs iFrom arrd iTo (Sz k) =
  withForeignArray arrs $ \ps ->
    withForeignArray arrd $ \pd ->
      copyWith (coerce (advancePtr ps iFrom)) (coerce (advancePtr pd iTo)) (fromIntegral k)
{-# INLINE copyWithForeignArray #-}


foldWithForeignArray ::
     (Coercible a b, Index ix)
  => (Ptr b -> CLong -> IO e)
  -> ForeignArray ix a
  -> IO e
foldWithForeignArray foldAction arr =
  withForeignArray arr $ \ptr ->
    foldAction (coerce ptr) (fromIntegral (unSz (lengthForeignArray arr)))
{-# INLINE foldWithForeignArray #-}


fold2WithForeignArray ::
     (Coercible a b, Coercible e c, Index ix)
  => (Ptr b -> Ptr b -> CLong -> IO e)
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO c
fold2WithForeignArray foldAction arr1 arr2 =
  let k = fromIntegral (unSz (lengthForeignArray arr1))
   in coerce $
      withForeignArray arr1 $ \p1 ->
        withForeignArray arr2 $ \p2 -> foldAction (coerce p1) (coerce p2) k
{-# INLINE fold2WithForeignArray #-}


eqWithForeignArray ::
     (Coercible a b, Index ix)
  => (Ptr b -> Ptr b -> CLong -> IO CBool)
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO Bool
eqWithForeignArray eqAction arr1 arr2 = fold2WithForeignArray eqWithPtrs arr1 arr2
  where
    eqWithPtrs p1 p2 _
      | p1 == p2 = pure True
      | otherwise = cboolToBool <$> eqAction p1 p2 (fromIntegral (unSz (lengthForeignArray arr1)))
    {-# INLINE eqWithPtrs #-}
{-# INLINE eqWithForeignArray #-}


zipWithForeignArray ::
     (Storable a, Index ix)
  => (Ptr b -> Ptr b -> Ptr b -> CLong -> IO ())
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO ()
zipWithForeignArray zipWithAction arr1 arr2 resArr =
  withForeignArray arr1 $ \p1 ->
    withForeignArray arr2 $ \p2 ->
      withForeignArray resArr $ \pRes ->
        let sz = fromIntegral (unSz (lengthForeignArray resArr))
         in zipWithAction (coerce p1) (coerce p2) (coerce pRes) sz
{-# INLINE zipWithForeignArray #-}

liftForeignArray ::
     (Storable a, Index ix)
  => (Ptr b -> Ptr b -> CLong -> IO ())
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO ()
liftForeignArray liftAction arr resArr =
  withForeignArray arr $ \p ->
    withForeignArray resArr $ \pRes ->
      let sz = fromIntegral (unSz (lengthForeignArray resArr))
       in liftAction (coerce p) (coerce pRes) sz
{-# INLINE liftForeignArray #-}



cboolToBool :: CBool -> Bool
cboolToBool = (/= 0)
{-# INLINE cboolToBool #-}



-- Arguments to the supplied action, eg. 4 elements per alignment:
--
--                          .- ptr (ForeignArray start)
--  beginning of           /    .- ptrAlignedAdjusted
--   ForeignPtr \         |    /                     .-- ForeignArray end
--               v        v   v                     /
--               x.x.x.x.[x.x|x.x.x.x|x.x.x.x|x.x.x].x.x.x.x.x <- end of allocated memory
--                       |\ / \___________   \      \____________
--                       | v              \___\                  \
--                       | lengthBefore:2      `lengthAligned:8   \
--                       \_________________________________________\
--                                                                  `lengthTotal:13
withAlignedForeignArray ::
     forall a c ix. (Storable a)
  => ForeignArray ix a
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> Sz1
  -> (Ptr a -> Int -> Ptr a -> Int -> IO c)
  -> IO c
withAlignedForeignArray arr perAlignment (Sz lengthTotal) action =
  withForeignArray arr $ \ptr -> do
    let !esize = sizeOf (undefined :: a)
        !ptrAligned = alignPtr ptr (perAlignment * esize)
        !lengthBefore = min lengthTotal ((ptrAligned `minusPtr` ptr) `div` esize)
        !lengthAligned = ((lengthTotal - lengthBefore) `div` perAlignment) * perAlignment
        !ptrAlignedAdjusted = ptr `advancePtr` lengthBefore
    action ptr lengthBefore ptrAlignedAdjusted lengthAligned
{-# INLINE withAlignedForeignArray #-}

applyAlignedForeignArray ::
     (Storable a, Index ix, Coercible a b, Coercible e c)
  => (e -> Ptr b -> CLong -> IO e)
  -> (c -> Ptr a -> IO c)
  -> c
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> IO c
applyAlignedForeignArray applyAligned apply initAcc perAlignment arr = do
  let sz = lengthForeignArray arr
      lengthTotal = unSz sz
  withAlignedForeignArray arr perAlignment sz $ \ptr lengthBefore ptrAligned lengthAligned -> do
    let applyLoop from to iAcc =
          loopM from (< to) (+ 1) iAcc $ \i acc -> apply acc (ptr `advancePtr` i)
        {-# INLINE applyLoop #-}
        ptrAdjusted = coerce ptrAligned
    beforeRes <- applyLoop 0 lengthBefore initAcc
    resAligned <- applyAligned (coerce beforeRes) ptrAdjusted (fromIntegral lengthAligned)
    applyLoop (lengthBefore + lengthAligned) lengthTotal (coerce resAligned)
{-# INLINE applyAlignedForeignArray #-}

apply2AlignedForeignArray ::
     (Storable a, Index ix, Coercible a b, Coercible e c)
  => (e -> Ptr b -> Ptr b -> CLong -> IO e)
  -> (c -> Ptr a -> Ptr a -> IO c)
  -> c
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO c
apply2AlignedForeignArray applyAligned apply initAcc perAlignment arr1 arr2 = do
  let sz = lengthForeignArray arr1
      lengthTotal = unSz sz
  withAlignedForeignArray arr1 perAlignment sz $ \ptr1 lengthBefore ptr1Aligned lengthAligned ->
    withForeignArray arr2 $ \ptr2 -> do
      let applyLoop from to iAcc =
            loopM from (< to) (+ 1) iAcc $ \i acc ->
              apply acc (ptr1 `advancePtr` i) (ptr2 `advancePtr` i)
          {-# INLINE applyLoop #-}
          ptr1Adjusted = coerce ptr1Aligned
          ptr2Adjusted = coerce (ptr2 `advancePtr` lengthBefore)
      resBefore <- applyLoop 0 lengthBefore initAcc
      resAligned <-
        applyAligned (coerce resBefore) ptr1Adjusted ptr2Adjusted (fromIntegral lengthAligned)
      applyLoop (lengthBefore + lengthAligned) lengthTotal (coerce resAligned)
{-# INLINE apply2AlignedForeignArray #-}


apply2AlignedForeignArray' ::
     (Storable a, Storable b, Index ix, Coercible a x, Coercible b y, Coercible e c)
  => (e -> Ptr x -> Ptr y -> CLong -> IO e)
  -> (c -> Ptr a -> Ptr b -> IO c)
  -> c
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> ForeignArray ix b
  -> IO c
apply2AlignedForeignArray' applyAligned apply initAcc perAlignment arr1 arr2 = do
  let sz = lengthForeignArray arr1
      lengthTotal = unSz sz
  withAlignedForeignArray arr1 perAlignment sz $ \ptr1 lengthBefore ptr1Aligned lengthAligned ->
    withForeignArray arr2 $ \ptr2 -> do
      let applyLoop from to iAcc =
            loopM from (< to) (+ 1) iAcc $ \i acc ->
              apply acc (ptr1 `advancePtr` i) (ptr2 `advancePtr` i)
          {-# INLINE applyLoop #-}
          ptr1Adjusted = coerce ptr1Aligned
          ptr2Adjusted = coerce (ptr2 `advancePtr` lengthBefore)
      resBefore <- applyLoop 0 lengthBefore initAcc
      resAligned <-
        applyAligned (coerce resBefore) ptr1Adjusted ptr2Adjusted (fromIntegral lengthAligned)
      applyLoop (lengthBefore + lengthAligned) lengthTotal (coerce resAligned)
{-# INLINE apply2AlignedForeignArray' #-}

apply3AlignedForeignArray ::
     (Storable a, Index ix, Coercible a b, Coercible e c)
  => (e -> Ptr b -> Ptr b -> Ptr b -> CLong -> IO e)
  -> (c -> Ptr a -> Ptr a -> Ptr a -> IO c)
  -> c
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO c
apply3AlignedForeignArray applyAligned apply initAcc perAlignment arr1 arr2 arr3 = do
  let sz = lengthForeignArray arr1
      lengthTotal = unSz sz
  withAlignedForeignArray arr1 perAlignment sz $ \ptr1 lengthBefore ptr1Aligned lengthAligned ->
    withForeignArray arr2 $ \ptr2 ->
      withForeignArray arr3 $ \ptr3 -> do
        let applyLoop from to iAcc =
              loopM from (< to) (+ 1) iAcc $ \i acc ->
                apply acc (ptr1 `advancePtr` i) (ptr2 `advancePtr` i) (ptr3 `advancePtr` i)
            {-# INLINE applyLoop #-}
            ptr1Adjusted = coerce ptr1Aligned
            ptr2Adjusted = coerce (ptr2 `advancePtr` lengthBefore)
            ptr3Adjusted = coerce (ptr3 `advancePtr` lengthBefore)
            clengthAligned = fromIntegral lengthAligned
        resBefore <- applyLoop 0 lengthBefore initAcc
        resAligned <-
          applyAligned (coerce resBefore) ptr1Adjusted ptr2Adjusted ptr3Adjusted clengthAligned
        applyLoop (lengthBefore + lengthAligned) lengthTotal (coerce resAligned)
{-# INLINE apply3AlignedForeignArray #-}


liftAlignedForeignArray ::
     (Storable a, Index ix, Coercible a b)
  => (Ptr b -> Ptr b -> CLong -> IO ())
  -> (a -> a)
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO ()
liftAlignedForeignArray liftAligned f =
  apply2AlignedForeignArray (const liftAligned) (\_ p1 p2 -> poke p2 . f =<< peek p1) ()
{-# INLINE liftAlignedForeignArray #-}

liftAlignedForeignArray' ::
     (Storable a, Storable b, Index ix, Coercible a x, Coercible b y)
  => (Ptr x -> Ptr y -> CLong -> IO ())
  -> (a -> b)
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> ForeignArray ix b
  -> IO ()
liftAlignedForeignArray' liftAligned f =
  apply2AlignedForeignArray' (const liftAligned) (\_ p1 p2 -> poke p2 . f =<< peek p1) ()
{-# INLINE liftAlignedForeignArray' #-}


zipWithAlignedForeignArray ::
     (Storable a, Index ix, Coercible a b, Show a, Num a, Eq a)
  => (Ptr b -> Ptr b -> Ptr b -> CLong -> IO ())
  -> (a -> a -> a)
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO ()
zipWithAlignedForeignArray zipWithAligned f =
  apply3AlignedForeignArray
    (const zipWithAligned)
    (\_ p1 p2 p3 -> f <$> peek p1 <*> peek p2 >>= poke p3)
    ()
{-# INLINE zipWithAlignedForeignArray #-}


foldWithAlignedForeignArray ::
     (Storable a, Coercible a b, Coercible e c, Index ix)
  => (e -> Ptr b -> CLong -> IO e)
  -> (c -> a -> c)
  -> c
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> IO c
foldWithAlignedForeignArray foldAligned foldUnaligned =
  applyAlignedForeignArray foldAligned (\ !acc !ptr -> foldUnaligned acc <$> peek ptr)
{-# INLINE foldWithAlignedForeignArray #-}


fold2WithAlignedForeignArray ::
     (Storable a, Coercible a b, Coercible e c, Index ix)
  => (e -> Ptr b -> Ptr b -> CLong -> IO e)
  -- ^ Folding SIMD action. Ensure that the first pointer is always aligned according to
  -- the supplied alignment
  -> (c -> a -> a -> c)
  -> c
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO c
fold2WithAlignedForeignArray foldAligned foldUnaligned =
  apply2AlignedForeignArray foldAligned (\ !acc !p1 !p2 -> foldUnaligned acc <$> peek p1 <*> peek p2)
{-# INLINE fold2WithAlignedForeignArray #-}


foldNonEmptyWithAlignedForeignArray ::
     (Storable a, Coercible a b, Coercible e a, Index ix)
  => (e -> Ptr b -> CLong -> IO e)
  -> (a -> a -> a)
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> IO a
foldNonEmptyWithAlignedForeignArray foldAligned foldUnaligned perAlignment arr = do
  e0 <- readForeignArray arr 0
  let arrNo0 = extractForeignArray 1 (lengthForeignArray arr - 1) arr
  foldWithAlignedForeignArray foldAligned foldUnaligned e0 perAlignment arrNo0
{-# INLINE foldNonEmptyWithAlignedForeignArray #-}


eqWithAlignedForeignArray ::
     (Eq a, Storable a, Coercible a b, Index ix)
  => (Ptr b -> Ptr b -> CLong -> IO CBool)
  -> Int -- ^ Alignement. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO Bool
eqWithAlignedForeignArray eqAction =
  fold2WithAlignedForeignArray eqWithPtrs (\acc x y -> acc && x == y) True
  where
    eqWithPtrs acc p1 p2 sz
      | p1 == p2 = pure True -- arrays are same whenever two pointers are equal, even when
                             -- they are adjusted for alignement
      | not acc = pure False
      | otherwise = cboolToBool <$> eqAction p1 p2 sz
    {-# INLINE eqWithPtrs #-}
{-# INLINE eqWithAlignedForeignArray #-}

castForeignArray :: ForeignArray ix a -> ForeignArray ix b
castForeignArray (ForeignArray sz flag ptr fptr) =
  ForeignArray sz flag (castPtr ptr) (castForeignPtr fptr)
{-# INLINE castForeignArray #-}



fillWithAlignedForeignArray ::
     (Index ix, Coercible a b, Storable a)
  => (b -> Ptr b -> CLong -> IO ())
  -> Int -- ^ Alignement. In number of elements, rather than bytes.
  -> a
  -> ForeignArray ix a
  -> IO ()
fillWithAlignedForeignArray fillAction perAlignment e =
  applyAlignedForeignArray (\_ -> fillAction (coerce e)) (\_ p -> poke p e) () perAlignment
{-# INLINE fillWithAlignedForeignArray #-}


copyWithAlignedForeignArray ::
     (Storable a, Coercible a b, Index ix)
  => (Ptr b -> Ptr b -> CLong -> IO ())
  -> Int -- ^ Alignement. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO ()
copyWithAlignedForeignArray copyAction =
  apply2AlignedForeignArray (const copyAction) (\ _ ptr1 ptr2 -> peek ptr1 >>= poke ptr2) ()
{-# INLINE copyWithAlignedForeignArray #-}

evenPowerSumAlignedForeignArray ::
     (Storable c, Index ix, Num c, Coercible e c)
  => (CLong -> e -> Ptr e -> CLong -> IO e)
  -> Int -- ^ Alignement. In number of elements, rather than bytes.
  -> ForeignArray ix c
  -> Int -- ^ Power
  -> IO c
evenPowerSumAlignedForeignArray action perAlignment arr pow =
  foldWithAlignedForeignArray (action (fromIntegral pow)) (powerSum pow) 0 perAlignment arr
{-# INLINE evenPowerSumAlignedForeignArray #-}

absPowerSumAlignedForeignArray ::
     (Storable c, Index ix, Num c, Coercible e c)
  => (CLong -> e -> Ptr e -> CLong -> IO e)
  -> Int -- ^ Alignement. In number of elements, rather than bytes.
  -> ForeignArray ix c
  -> Int -- ^ AbsPower
  -> IO c
absPowerSumAlignedForeignArray action perAlignment arr pow =
  foldWithAlignedForeignArray
    (action (fromIntegral pow))
    (\acc -> powerSum pow acc . abs)
    0
    perAlignment
    arr
{-# INLINE absPowerSumAlignedForeignArray #-}

powerSum :: (Integral b, Num a) => b -> a -> a -> a
powerSum pow acc x = acc + x ^ pow
{-# INLINE powerSum #-}

multiplySumAlignedForeignArray ::
     (Storable a, Index ix, Num a, Coercible a b, Coercible e a)
  => (e -> Ptr b -> Ptr b -> CLong -> IO e)
  -> Int -- ^ Alignement. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO a
multiplySumAlignedForeignArray multiplySumAligned =
  fold2WithAlignedForeignArray multiplySumAligned (\acc x y -> acc + x * y) 0
{-# INLINE multiplySumAlignedForeignArray #-}
