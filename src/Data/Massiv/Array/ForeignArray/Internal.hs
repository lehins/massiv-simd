{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- |
-- Module      : Data.Massiv.Array.ForeignArray.Internal
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.ForeignArray.Internal
  ( ForeignArray(..)
  , lengthForeignArray
  , castForeignArray
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
  -- -- , reallocAlignedForeignArray
  , isSameForeignArray
  , withForeignArray
  , readForeignArray
  , writeForeignArray
  , sliceForeignArray
  , extractForeignArray
  ) where

import Control.DeepSeq
import Control.Monad.Primitive
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

data ForeignArrayContents
  = MallocArray {-# UNPACK #-} !(Ptr CBool) Addr# ForeignPtrContents
  | HaskellArray ForeignPtrContents

instance NFData ForeignArrayContents where
  rnf =
    \case
      MallocArray _flag _addr# contents -> seqContents contents
      HaskellArray contents -> seqContents contents
    where
      seqContents =
        \case
          PlainForeignPtr f -> f `seq` ()
          MallocPtr _mba# f -> f `seq` ()
          PlainPtr _mba# -> ()


data ForeignArray ix e = ForeignArray
  { foreignArraySize     :: !(Sz ix)
  -- ^ Size of the array. Can be less than the actual memory allocated
  --
  -- @since 0.1.0
  , foreignArrayPtr      :: Addr#
  -- ^ Pointer to the beginning of the data, as far as this array is concerned, which
  -- means it does not necesserally points to the beginning of the allocated memory.
  --
  -- @since 0.1.0
  , foreignArrayContents :: ForeignArrayContents
  -- ^ Pinned `MutableByteArray` that contains the data.
  --
  -- @since 0.1.0
  }


instance NFData ix => NFData (ForeignArray ix e) where
  rnf (ForeignArray sz _addr# contents) = sz `deepseq` contents `deepseq` ()

foreign import ccall unsafe "massiv.c &massiv_free_flagged"
  finalizerFreeFlagged :: FinalizerEnvPtr CBool a




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
allocForeignArray allocArray sz = fromMallocPtr sz =<< allocArray (totalElem sz)
{-# INLINE allocForeignArray #-}

-- | Helper pointer converter.
fromMallocPtr :: Sz ix -> Ptr e -> IO (ForeignArray ix e)
fromMallocPtr sz ptr = do
  fptr@(ForeignPtr addr# _) <- newForeignPtr_ ptr
  contents <- toForeignArrayContents fptr
  case contents of
    MallocArray flag _ _ -> addForeignPtrFinalizerEnv finalizerFreeFlagged flag fptr
    _ -> pure ()
  pure $ ForeignArray sz addr# contents
{-# INLINE fromMallocPtr #-}


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
reallocForeignArray farr@(ForeignArray sz curPtr# contents) newsz
  | newLength == oldLength = pure (ForeignArray newsz curPtr# contents)
  | otherwise =
    let elementSize = sizeOf (undefined :: e)
        getOffsetBytes :: Ptr e -> Int
        getOffsetBytes = minusPtr (Ptr curPtr#)
        getOffsetBytesMBA :: MutableByteArray# RealWorld -> Int
        getOffsetBytesMBA mba# = getOffsetBytes (Ptr (byteArrayContents# (unsafeCoerce# mba#)))
        reallocMutableByteArray alloc mba#
          | newLength <= oldLength = do
            let !(I# newFullByteSize#) = newByteSize + offsetBytes
            primitive_ (shrinkMutableByteArray# mba# newFullByteSize#)
            pure (ForeignArray newsz curPtr# contents)
          | otherwise = do
            let !(I# oldByteSize#) = oldLength * elementSize
            fptr <- alloc newByteSize (alignment (undefined :: e))
              -- We can't grow pinned memory in ghc, so we are left with manual create new
              -- and copy appraoch.
            withForeignPtr fptr $ \(Ptr addr#) -> do
              primitive_ (copyMutableByteArrayToAddr# mba# offsetBytes# addr# oldByteSize#)
              ForeignArray newsz addr# <$> toForeignArrayContents fptr
          where
            !offsetBytes@(I# offsetBytes#) = getOffsetBytesMBA mba#
            !newByteSize = newLength * elementSize
        resizeContents ptrContents =
          case ptrContents of
            PlainForeignPtr _ -> do
              newfarr <- mallocForeignArray newsz
              copyForeignArray (extractForeignArray 0 newsz farr) newfarr
              pure newfarr
            MallocPtr mba# _ -> reallocMutableByteArray mallocForeignPtrAlignedBytes mba#
            PlainPtr mba# -> reallocMutableByteArray mallocPlainForeignPtrAlignedBytes mba#
     in case contents of
          MallocArray flag addr# c@(PlainForeignPtr _) -> do
            let ptr = Ptr addr# :: Ptr e
                offsetBytes = getOffsetBytes ptr
                offset = offsetBytes `div` elementSize
            rPtr <- reallocArray ptr (newLength + offset)
            if rPtr == ptr -- shrunk or grew without copy, so we can keep the same `fptr`
              then pure (ForeignArray newsz curPtr# contents)
              else do
                poke flag 1 -- ensure that the finilizer does not double free
                touch c -- ensure that above usage of reallocArray was safe
                fromMallocPtr newsz rPtr
          MallocArray _ _ ptrContents -> resizeContents ptrContents
          HaskellArray ptrContents -> resizeContents ptrContents
  where
    !oldLength = totalElem sz
    !newLength = totalElem newsz
{-# INLINE reallocForeignArray #-}


-- | Check if two foreign arrays refer to the same memory block.
--
-- @since 0.1.0
isSameForeignArray :: ForeignArray ix1 e1 -> ForeignArray ix2 e2 -> Bool
isSameForeignArray (ForeignArray _ _ c1) (ForeignArray _ _ c2) =
  case (c1, c2) of
    (MallocArray _ p1# _, MallocArray _ p2# _) -> Ptr p1# == Ptr p2#
    (HaskellArray fc1, HaskellArray fc2) ->
      case (fc1, fc2) of
        (MallocPtr mba1# _, MallocPtr mba2# _) ->
          sameMutableByteArray (MutableByteArray mba1#) (MutableByteArray mba2#)
        (PlainPtr mba1#, PlainPtr mba2#) ->
          sameMutableByteArray (MutableByteArray mba1#) (MutableByteArray mba2#)
        _ -> False
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


-- -------------
-- -- Aligned --
-- -------------

-- | Same as `newForeignArray`, but with ability to specify custom alignment.
--
-- @since 0.1.0
newAlignedForeignArray ::
     forall e ix. (Storable e, Index ix) => Sz ix -> Int -> IO (ForeignArray ix e)
newAlignedForeignArray sz align = do
  let dummy = undefined :: e
  fptr@(ForeignPtr ptr# _) <- mallocPlainForeignPtrAlignedBytes (totalElem sz * sizeOf dummy) align
  arrayContents <- toForeignArrayContents fptr
  pure $ ForeignArray sz ptr# arrayContents
{-# INLINE newAlignedForeignArray #-}

toForeignArrayContents :: ForeignPtr a -> IO ForeignArrayContents
toForeignArrayContents (ForeignPtr ptr contents) =
  case contents of
    PlainForeignPtr _ -> do
      flag <- calloc
      pure $ MallocArray flag ptr contents
    _ -> pure $ HaskellArray contents

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
  ForeignArray _ _ contents <- fromMallocPtr sz ptr
  let !(Ptr addr#) = alignPtr ptr align
  pure $ ForeignArray sz addr# contents
{-# INLINE allocAlignedForeignArray #-}


-- | Read en element from an array at a linear index. No bounds checking is performed.
--
-- @since 0.1.0
readForeignArray :: (Index ix, Storable e) => ForeignArray ix e -> Ix1 -> IO e
readForeignArray =
  INDEX_CHECK("ForeignArray.readForeignArray",
              lengthForeignArray,
              \ arr i -> withForeignArray arr $ \curPtr -> peek (advancePtr curPtr i))
{-# INLINE readForeignArray #-}

-- | Write an element to an array at a linear index. No bounds checking is performed.
--
-- @since 0.1.0
writeForeignArray :: (Index ix, Storable e) => ForeignArray ix e -> Ix1 -> e -> IO ()
writeForeignArray =
  INDEX_CHECK("ForeignArray.writeForeignArray",
              lengthForeignArray,
              \ arr i e -> withForeignArray arr $ \curPtr -> poke (advancePtr curPtr i) e)
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
sliceForeignArray (ForeignArray sz curPtr# contents) i =
  let !(Ptr newPtr#) = advancePtr (Ptr curPtr#) offset :: Ptr e
      !offset = toLinearIndex sz (consDim i (zeroIndex :: Lower ix))
   in ForeignArray (snd (unconsSz sz)) newPtr# contents
{-# INLINE sliceForeignArray #-}


-- | Adjust the size and move the pointer by an offset. Does no bounds checking and no
-- memory reallocation, just some pointer manipulations.
--
-- @since 0.1.0
extractForeignArray ::
     forall ix ix' e. Storable e
  => Ix1 -- ^ Offset
  -> Sz ix -- ^ New size
  -> ForeignArray ix' e
  -> ForeignArray ix e
extractForeignArray i newsz (ForeignArray _ curAddr# fptr) =
  let !(Ptr newAddr#) = advancePtr (Ptr curAddr#) i :: Ptr e
   in ForeignArray newsz newAddr# fptr
{-# INLINE extractForeignArray #-}

-- | Access the pointer to the beginning of this array.
--
-- @since 0.1.0
withForeignArray :: ForeignArray ix e -> (Ptr e -> IO a) -> IO a
withForeignArray farr io = do
  r <- io (unsafeForeignArrayToPtr farr)
  touchForeignArray farr
  pure r


-- -- | Access the pointer to the beginning of this array.
-- --
-- -- @since 0.1.0
-- withForeignArrayPtr :: ForeignArray ix e -> (Ptr e -> IO a) -> IO a
-- withForeignArrayPtr farr io = do
--   r <- io (unsafeForeignArrayToPtr farr)
--   touchForeignArray farr
--   pure r

-- | Get the plain pointer to the beginning of the array. Use `withForeignArray` instead.
--
-- @since 0.1.0
unsafeForeignArrayToPtr :: ForeignArray ix e -> Ptr a
unsafeForeignArrayToPtr (ForeignArray _ addr# _) = Ptr addr#


-- | Make sure that the array is still alive.
--
-- @since 0.1.0
touchForeignArray :: ForeignArray ix e -> IO ()
touchForeignArray (ForeignArray _ _ c) = touch c

-- | Get the total number of elements in the array
--
-- @since 0.1.0
lengthForeignArray :: Index ix => ForeignArray ix e -> Sz1
lengthForeignArray (ForeignArray sz _ _) = SafeSz (totalElem sz)
{-# INLINE lengthForeignArray #-}

-- | Cast an array from one type of elements to another.
--
-- @since 0.1.0
castForeignArray :: ForeignArray ix a -> ForeignArray ix b
castForeignArray (ForeignArray sz addr# contents) = ForeignArray sz addr# contents
{-# INLINE castForeignArray #-}
