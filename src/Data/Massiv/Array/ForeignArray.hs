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
  , setWithForeignArray
  , copyWithForeignArray
  -- ** Folding
  , eqWithForeignArray
  , foldWithForeignArray
  , fold2WithForeignArray
  , zipWithAlignedForeignArray
  , foldWithAlignedForeignArray
  , fold2WithAlignedForeignArray
  -- ** Combining
  , zipWithForeignArray
  ) where

import Control.Monad
import Control.Monad.Primitive
import Control.DeepSeq
import Data.Coerce
import Data.Primitive.ByteArray
import Data.Massiv.Array.Unsafe (Sz(SafeSz))
import Data.Massiv.Core.Index
import Foreign.C
import Foreign.ForeignPtr
import GHC.ForeignPtr
import GHC.Ptr
import GHC.Exts
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
--import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (mapM)

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


foreign import ccall unsafe "massiv.c &free_flagged" finalizerFreeFlagged :: FinalizerEnvPtr CBool a


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


-- -- | Shrink or grow an array.
-- --
-- -- @since 0.1.0
-- reallocAlignedForeignArray ::
--      forall ix' ix e. (Index ix', Index ix, Storable e)
--   => ForeignArray ix' e
--   -> Sz ix -- ^ New size. Can be bigger or smaller
--   -> Int
--   -> IO (ForeignArray ix e)
-- reallocAlignedForeignArray (ForeignArray oldsz freed curPtr fptr) newsz align
--   | oldk == newk
--   withForeignPtr fptr $ \ptr -> do
--     let esize = sizeOf (undefined :: e)
--     let offset = curPtr `minusPtr` ptr
--     rPtr <- reallocBytes ptr (offset + totalElem newsz * esize)
--     if rPtr == ptr
--       then pure $ ForeignArray newsz freed curPtr fptr
--       else do
--         poke freed 1
--         freed' <- calloc
--         fp' <- newForeignPtrEnv finalizerFreeFlagged freed' rPtr
--         pure $ ForeignArray newsz freed' (advancePtr rPtr offset) fp'
--   where
--     oldk = totalElem oldsz
--     newk = totalElem newsz
-- {-# INLINE reallocAlignedForeignArray #-}


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


-- fold2WithForeignArray ::
--      (Coercible a b, Index ix)
--   => (Ptr b -> Ptr b -> CLong -> IO e)
--   -> ForeignArray ix a
--   -> ForeignArray ix a
--   -> IO e
-- fold2WithForeignArray foldAction arr1 arr2 =
--   withForeignArray arr1 $ \p1 ->
--     withForeignArray arr2 $ \p2 ->
--       foldAction
--         (coerce p1)
--         (coerce p2)
--         (fromIntegral
--            (unSz (lengthForeignArray arr1) `min` unSz (lengthForeignArray arr2)))
-- {-# INLINE fold2WithForeignArray #-}

fold2WithForeignArray ::
     (Coercible a b, Coercible e c, Index ix)
  => (Ptr b -> Ptr b -> CLong -> IO e)
  -> Sz1
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO c
fold2WithForeignArray foldAction sz arr1 arr2 =
  coerce $ withForeignArray arr1 $ \p1 ->
    withForeignArray arr2 $ \p2 -> foldAction (coerce p1) (coerce p2) (fromIntegral (unSz sz))
{-# INLINE fold2WithForeignArray #-}


eqWithForeignArray ::
     (Coercible a b, Index ix)
  => (Ptr b -> Ptr b -> CLong -> IO CBool)
  -> Sz1
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO Bool
eqWithForeignArray eqAction sz arr1 arr2 = fold2WithForeignArray eqWithPtrs sz arr1 arr2
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



cboolToBool :: CBool -> Bool
cboolToBool = (/= 0)
{-# INLINE cboolToBool #-}



-- Arguments to the supplied action:
--
--                          .- ptr (ForeignArray start)
--  beginning of           /    .- ptrAlignedAdjusted
--   ForeignPtr \         |    /                     .-- ForeignArray end
--               v        v   v                     /
--               x.x.x.x.[x.x|x.x.x.x|x.x.x.x|x.x.x].x.x.x.x.x <- end of allocated memory
--                       |\ / \___________   \      \____________
--                       | v              \___\                  \
--                       | `lengthBefore:2     `lengthAligned:8   \
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

zipWithAlignedForeignArray ::
     (Storable a, Index ix, Coercible a b, Show a, Num a, Eq a)
  => (Ptr b -> Ptr b -> Ptr b -> CLong -> IO ())
  -> (a -> a -> a)
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO ()
zipWithAlignedForeignArray zipWithSIMD f perAlignment arr1 arr2 resArr = do
  let sz = lengthForeignArray resArr
      lengthTotal = unSz sz
  withAlignedForeignArray arr1 perAlignment sz $ \ptr1 lengthBefore ptr1Aligned lengthAligned ->
    withForeignArray arr2 $ \ptr2 ->
      withForeignArray resArr $ \ptrRes -> do
        let zipWithOverEdge from to =
              loopM_ from (< to) (+ 1) $ \i -> do
                e1 <- peek (ptr1 `advancePtr` i)
                e2 <- peek (ptr2 `advancePtr` i)
                poke (ptrRes `advancePtr` i) $! f e1 e2
            {-# INLINE zipWithOverEdge #-}
            ptr1Adjusted = coerce ptr1Aligned
            ptr2Adjusted = coerce (ptr2 `advancePtr` lengthBefore)
            ptrResAdjusted = coerce (ptrRes `advancePtr` lengthBefore)
        zipWithOverEdge 0 lengthBefore
        zipWithSIMD ptr1Adjusted ptr2Adjusted ptrResAdjusted (fromIntegral lengthAligned)
        zipWithOverEdge (lengthBefore + lengthAligned) lengthTotal
{-# INLINE zipWithAlignedForeignArray #-}


foldWithAlignedForeignArray ::
     forall a b c e ix. (Storable a, Coercible a b, Coercible e c, Index ix)
  => (e -> Ptr b -> CLong -> IO e)
  -> (c -> a -> c)
  -> c
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> IO c
foldWithAlignedForeignArray foldActionSIMD foldWith initAcc perAlignment arr = do
  let sz = lengthForeignArray arr
  withAlignedForeignArray arr perAlignment sz $ \ptr lengthBefore ptrAligned lengthAligned -> do
    let !lengthTotal = unSz sz
        foldOverEdge from to iAcc =
          loopM from (< to) (+ 1) iAcc $ \ !i !acc -> foldWith acc <$> peek (ptr `advancePtr` i)
        {-# INLINE foldOverEdge #-}
    preRes <- foldOverEdge 0 lengthBefore initAcc
    resAcc <-
      coerce $ foldActionSIMD (coerce preRes) (coerce ptrAligned) (fromIntegral lengthAligned)
    foldOverEdge (lengthBefore + lengthAligned) lengthTotal resAcc
{-# INLINE foldWithAlignedForeignArray #-}


fold2WithAlignedForeignArray ::
     forall a b c e ix. (Storable a, Coercible a b, Coercible e c, Index ix)
  => (e -> Ptr b -> Ptr b -> CLong -> IO e)
  -- ^ Folding SIMD action. Ensure that the first pointer is always aligned according to
  -- the supplied alignment
  -> (c -> a -> a -> c)
  -> c
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> Sz1
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO c
fold2WithAlignedForeignArray foldActionSIMD foldWith initAcc perAlignment sz arr1 arr2 =
  withAlignedForeignArray arr1 perAlignment sz $ \ptr1 lengthBefore ptr1Aligned lengthAligned ->
    withForeignArray arr2 $ \ptr2 -> do
      let lengthTotal = unSz sz
          ptr1Adjusted = coerce ptr1Aligned
          ptr2Adjusted = coerce (ptr2 `advancePtr` lengthBefore)
          foldOverEdge from to iAcc =
            loopM from (< to) (+ 1) iAcc $ \ !i !acc ->
              foldWith acc <$> peek (ptr1 `advancePtr` i) <*> peek (ptr2 `advancePtr` i)
          {-# INLINE foldOverEdge #-}
      resBefore <- foldOverEdge 0 lengthBefore initAcc
      resAcc <-
        coerce $
        foldActionSIMD (coerce resBefore) ptr1Adjusted ptr2Adjusted (fromIntegral lengthAligned)
      foldOverEdge (lengthBefore + lengthAligned) lengthTotal resAcc
{-# INLINE fold2WithAlignedForeignArray #-}
