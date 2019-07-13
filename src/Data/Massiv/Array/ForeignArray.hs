{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
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
  , mallocForeignArray
  , callocForeignArray
  , reallocForeignArray
  , readForeignArray
  , writeForeignArray
  , sliceForeignArray
  , extractForeignArray
  ) where

import Data.Massiv.Core.Index
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (mapM)


#include "massiv.h"


data ForeignArray ix e = ForeignArray
  { foreignArraySize       :: !(Sz ix)
  , foreignArrayFreeFlag   :: {-# UNPACK #-}!(Ptr CBool)
  , foreignArrayPtr        :: {-# UNPACK #-}!(Ptr e)
  , foreignArrayForeignPtr :: !(ForeignPtr e)
  }


foreign import ccall unsafe "massiv.c &free_flagged" finalizerFreeFlagged :: FinalizerEnvPtr CBool a


-- | Allocate an array, but do not initialize any elements.
--
-- @since 0.1.0
mallocForeignArray ::
     (Storable e, Index ix) => Sz ix -> IO (ForeignArray ix e)
mallocForeignArray = allocForeignArray mallocArray
{-# INLINE mallocForeignArray #-}

-- | Allocate an array and initialize all elements to zero
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

-- | Shrink or grow an array.
--
-- @since 0.1.0
reallocForeignArray ::
     (Index ix, Storable e)
  => ForeignArray ix e
  -> Sz ix -- ^ New size. Can be bigger or smaller
  -> IO (ForeignArray ix e)
reallocForeignArray (ForeignArray _sz freed curPtr fptr) newsz =
  withForeignPtr fptr $ \ptr -> do
    let offset = (curPtr `minusPtr` ptr) `div` sizeOf (undefined :: Double)
    rPtr <- reallocArray ptr (offset + totalElem newsz)
    if rPtr == ptr
      then pure $ ForeignArray newsz freed curPtr fptr
      else do
        poke freed 1
        freed' <- calloc
        fp' <- newForeignPtrEnv finalizerFreeFlagged freed' rPtr
        pure $ ForeignArray newsz freed' (advancePtr rPtr offset) fp'
{-# INLINE reallocForeignArray #-}


-- | Read en element from an array at a linear index. No bounds checking is performed.
--
-- @since 0.1.0
readForeignArray :: (Index ix, Storable e) => ForeignArray ix e -> Ix1 -> IO e
readForeignArray (ForeignArray sz _ curPtr fptr) =
  INDEX_CHECK("ForeignArray.readForeignArray", Sz . totalElem, \ _ i -> withForeignPtr fptr $ \_ -> peek (advancePtr curPtr i)) sz
{-# INLINE readForeignArray #-}

-- | Write an element to an array at a linear index. No bounds checking is performed.
--
-- @since 0.1.0
writeForeignArray :: (Index ix, Storable e) => ForeignArray ix e -> Ix1 -> e -> IO ()
writeForeignArray (ForeignArray sz _ curPtr fptr) =
  INDEX_CHECK("ForeignArray.writeForeignArray", Sz . totalElem, \ _ i e -> withForeignPtr fptr $ \_ -> poke (advancePtr curPtr i) e) sz
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
  -> ForeignArray ix e
  -> ForeignArray ix e
extractForeignArray i newsz (ForeignArray _ freed curPtr fptr) =
  ForeignArray newsz freed (advancePtr curPtr i) fptr
{-# INLINE extractForeignArray #-}
