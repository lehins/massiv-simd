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
  , extractForeignArray
  ) where

import Control.DeepSeq
import Control.Monad.Primitive
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


data ForeignArray e = ForeignArray
  { foreignArrayPtr      :: Addr#
  -- ^ Pointer to the beginning of the data, as far as this array is concerned, which
  -- means it does not necesserally points to the beginning of the allocated memory.
  --
  -- @since 0.1.0
  , foreignArrayContents :: ForeignArrayContents
  -- ^ Pinned `MutableByteArray` that contains the data.
  --
  -- @since 0.1.0
  }


instance NFData (ForeignArray e) where
  rnf (ForeignArray _addr# contents) = contents `deepseq` ()

foreign import ccall unsafe "massiv.c &massiv_free_flagged"
  finalizerFreeFlagged :: FinalizerEnvPtr CBool a




-- | Allocate pinned memory for the array on the GHC heap, but do not initialize any
-- elements.
--
-- @since 0.1.0
newForeignArray ::
     forall e . Storable e => Sz1 -> IO (ForeignArray e)
newForeignArray sz = newAlignedForeignArray sz (alignment (undefined :: e))
{-# INLINE newForeignArray #-}

-- | Allocate an array using @malloc@, but do not initialize any elements.
--
-- @since 0.1.0
mallocForeignArray ::
     forall e . Storable e => Sz1 -> IO (ForeignArray e)
mallocForeignArray = allocForeignArray mallocArray
{-# INLINE mallocForeignArray #-}


-- | Allocate an array using @calloc@, thus also initialize all elements to zero
--
-- @since 0.1.0
callocForeignArray ::
     Storable e => Sz1 -> IO (ForeignArray e)
callocForeignArray = allocForeignArray callocArray
{-# INLINE callocForeignArray #-}


-- | Helper array allocator
allocForeignArray ::
     (Int -> IO (Ptr e)) -> Sz1 -> IO (ForeignArray e)
allocForeignArray allocArray (Sz sz) = fromMallocPtr =<< allocArray sz
{-# INLINE allocForeignArray #-}

-- | Helper pointer converter.
fromMallocPtr :: Ptr e -> IO (ForeignArray e)
fromMallocPtr ptr = do
  fptr@(ForeignPtr addr# _) <- newForeignPtr_ ptr
  contents <- toForeignArrayContents fptr
  case contents of
    MallocArray flag _ _ -> addForeignPtrFinalizerEnv finalizerFreeFlagged flag fptr
    _ -> pure ()
  pure $ ForeignArray addr# contents
{-# INLINE fromMallocPtr #-}

-- TODO: When growing, check if it is a slice and move the data instead.
-- TODO: Split grow and shrink, while keeping a general ralloc
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
     forall e . Storable e
  => Sz1 -- ^ Old size
  -> ForeignArray e
  -> Sz1 -- ^ New size. Can be bigger or smaller
  -> IO (ForeignArray e)
reallocForeignArray sz farr@(ForeignArray curPtr# contents) newsz
  | newLength == oldLength = pure (ForeignArray curPtr# contents)
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
            pure (ForeignArray curPtr# contents)
          | otherwise = do
            let !(I# oldByteSize#) = oldLength * elementSize
            fptr <- alloc newByteSize (alignment (undefined :: e))
              -- We can't grow pinned memory in ghc, so we are left with manual create new
              -- and copy appraoch.
            withForeignPtr fptr $ \(Ptr addr#) -> do
              primitive_ (copyMutableByteArrayToAddr# mba# offsetBytes# addr# oldByteSize#)
              ForeignArray addr# <$> toForeignArrayContents fptr
          where
            !offsetBytes@(I# offsetBytes#) = getOffsetBytesMBA mba#
            !newByteSize = newLength * elementSize
        resizeContents ptrContents =
          case ptrContents of
            PlainForeignPtr _ -> do
              newfarr <- mallocForeignArray newsz
              copyForeignArray sz (extractForeignArray 0 farr) newfarr
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
              then pure (ForeignArray curPtr# contents)
              else do
                poke flag 1 -- ensure that the finilizer does not double free
                touch c -- ensure that above usage of reallocArray was safe
                fromMallocPtr rPtr
          MallocArray _ _ ptrContents -> resizeContents ptrContents
          HaskellArray ptrContents -> resizeContents ptrContents
  where
    !oldLength = totalElem sz
    !newLength = totalElem newsz
{-# INLINE reallocForeignArray #-}


-- | Check if two foreign arrays refer to the same memory block.
--
-- @since 0.1.0
isSameForeignArray :: ForeignArray e1 -> ForeignArray e2 -> Bool
isSameForeignArray (ForeignArray _ c1) (ForeignArray _ c2) =
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
     Storable e
  => Sz1 -- ^ Number of elements to copy
  -> ForeignArray e -- ^ Source array
  -> ForeignArray e -- ^ Destination array
  -> IO ()
copyForeignArray sz arrSrc arrDest =
  withForeignArray arrSrc $ \ ptrSrc ->
    withForeignArray arrDest $ \ ptrDest ->
      copyArray ptrDest ptrSrc (unSz sz)
{-# INLINE copyForeignArray #-}

-- | Copy one array into another with @memmove@. Copied areas may overlap.
moveForeignArray ::
     Storable e
  => Sz1
  -> ForeignArray e -- ^ Source array
  -> ForeignArray e -- ^ Destination array
  -> IO ()
moveForeignArray sz arrSrc arrDest =
  withForeignArray arrSrc $ \ptrSrc ->
    withForeignArray arrDest $ \ptrDest ->
      moveArray ptrDest ptrSrc (unSz sz)
{-# INLINE moveForeignArray #-}

-- | Fill an array with the same byte using @memset@.
fillForeignArray ::
     Storable e
  => Sz1
  -> ForeignArray e -- ^ Source array
  -> Word8 -- ^ Byte value to use for filling the elements
  -> IO ()
fillForeignArray sz arr w8 =
  withForeignArray arr $ \ptr -> fillBytes ptr w8 (getsz arr undefined)
  where
    getsz :: Storable e => ForeignArray e -> e -> Int
    getsz _ dummy = unSz sz * sizeOf dummy
{-# INLINE fillForeignArray #-}


-- -------------
-- -- Aligned --
-- -------------

-- | Same as `newForeignArray`, but with ability to specify custom alignment.
--
-- @since 0.1.0
newAlignedForeignArray ::
     forall e . Storable e => Sz1 -> Int -> IO (ForeignArray e)
newAlignedForeignArray sz align = do
  let dummy = undefined :: e
  fptr@(ForeignPtr ptr# _) <- mallocPlainForeignPtrAlignedBytes (unSz sz * sizeOf dummy) align
  arrayContents <- toForeignArrayContents fptr
  pure $ ForeignArray ptr# arrayContents
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
mallocAlignedForeignArray :: Storable e => Sz1 -> Int -> IO (ForeignArray e)
mallocAlignedForeignArray = allocAlignedForeignArray mallocBytes
{-# INLINE mallocAlignedForeignArray #-}


-- | Allocate an array, but do not initialize any elements.
--
-- @since 0.1.0
callocAlignedForeignArray :: Storable e => Sz1 -> Int -> IO (ForeignArray e)
callocAlignedForeignArray = allocAlignedForeignArray mallocBytes
{-# INLINE callocAlignedForeignArray #-}


allocAlignedForeignArray ::
     forall e. Storable e
  => (Int -> IO (Ptr e))
  -> Sz1
  -> Int
  -> IO (ForeignArray e)
allocAlignedForeignArray allocBytes sz align = do
  ptr <- allocBytes (sizeOf (undefined :: e) * unSz sz + align - 1)
  ForeignArray _ contents <- fromMallocPtr ptr
  let !(Ptr addr#) = alignPtr ptr align
  pure $ ForeignArray addr# contents
{-# INLINE allocAlignedForeignArray #-}


-- | Read en element from an array at a linear index. No bounds checking is performed.
--
-- @since 0.1.0
readForeignArray :: Storable e => Sz1 -> ForeignArray e -> Ix1 -> IO e
readForeignArray _sz =
  INDEX_CHECK("ForeignArray.readForeignArray",
              const _sz,
              \ arr i -> withForeignArray arr $ \curPtr -> peek (advancePtr curPtr i))
{-# INLINE readForeignArray #-}

-- | Write an element to an array at a linear index. No bounds checking is performed.
--
-- @since 0.1.0
writeForeignArray :: Storable e => Sz1 -> ForeignArray e -> Ix1 -> e -> IO ()
writeForeignArray _sz =
  INDEX_CHECK("ForeignArray.writeForeignArray",
              const _sz,
              \ arr i e -> withForeignArray arr $ \curPtr -> poke (advancePtr curPtr i) e)
{-# INLINE writeForeignArray #-}


-- | Adjust the size and move the pointer by an offset. Does no bounds checking and no
-- memory reallocation, just some pointer manipulations.
--
-- @since 0.1.0
extractForeignArray ::
     forall e. Storable e
  => Ix1 -- ^ Offset
  -> ForeignArray e
  -> ForeignArray e
extractForeignArray i (ForeignArray curAddr# fptr) =
  let !(Ptr newAddr#) = advancePtr (Ptr curAddr#) i :: Ptr e
   in ForeignArray newAddr# fptr
{-# INLINE extractForeignArray #-}

-- | Access the pointer to the beginning of this array.
--
-- @since 0.1.0
withForeignArray :: ForeignArray e -> (Ptr e -> IO a) -> IO a
withForeignArray farr io = do
  r <- io (unsafeForeignArrayToPtr farr)
  touchForeignArray farr
  pure r


-- | Get the plain pointer to the beginning of the array. Use `withForeignArray` instead.
--
-- @since 0.1.0
unsafeForeignArrayToPtr :: ForeignArray e -> Ptr a
unsafeForeignArrayToPtr (ForeignArray addr# _) = Ptr addr#


-- | Make sure that the array is still alive.
--
-- @since 0.1.0
touchForeignArray :: ForeignArray e -> IO ()
touchForeignArray (ForeignArray _ c) = touch c

-- | Cast an array from one type of elements to another.
--
-- @since 0.1.0
castForeignArray :: ForeignArray a -> ForeignArray b
castForeignArray (ForeignArray addr# contents) = ForeignArray addr# contents
{-# INLINE castForeignArray #-}
