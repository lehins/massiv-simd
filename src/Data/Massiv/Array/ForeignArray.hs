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
  , sizeForeignArray
  , mallocForeignArray
  , callocForeignArray
  , reallocForeignArray
  , withForeignArray
  , readForeignArray
  , writeForeignArray
  , sliceForeignArray
  , extractForeignArray
  ) where

import Data.Massiv.Core.Index
import Data.Massiv.Array.Unsafe (Sz(SafeSz))
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (mapM)
import Control.DeepSeq

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
  rnf (ForeignArray sz flag ptr fptr) = sz `deepseq` flag `deepseq` ptr `deepseq` fptr `seq` ()


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
writeForeignArray =
  INDEX_CHECK("ForeignArray.writeForeignArray", sizeForeignArray, \ arr i e -> withForeignArray arr $ \ptr -> poke (advancePtr ptr i) e)
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
sizeForeignArray :: Index ix => ForeignArray ix e -> Sz1
sizeForeignArray (ForeignArray sz _ _ _) = SafeSz (totalElem sz)
{-# INLINE sizeForeignArray #-}
