{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Massiv.Array.SIMD
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.SIMD.Internal
  ( F(..)
  , Array(..)
  , withMFArrayPtr
  , unsafeWithFArrayPtr
  ) where

import Control.Monad.Primitive
import Data.Massiv.Array as A
import Data.Massiv.Array.ForeignArray
import Data.Massiv.Array.Unsafe
import Data.Massiv.Core.List
import Foreign.Ptr
import Prelude hiding (mapM)


-- | Foreign Array Representation suitable for passing to C/C++ over Foreign Function
-- Interface (FFI). Used for SIMD vectorization with Intel Intrinsics.
data F = F deriving Show

data instance Array F ix e = FArray
  { vComp  :: !Comp
  , vSize  :: !(Sz ix)
  , vArray :: !(ForeignArray e)
  }

-- | Access the pointer to the first element of the array. It is unsafe to mutate the
-- pointer, unless no one else is holding a reference to this array, or any other
-- parrent array if that one was a result of a slice.
--
-- @since 0.1.0
unsafeWithFArrayPtr :: Array F ix e -> (Ptr e -> IO a) -> IO a
unsafeWithFArrayPtr (FArray _ _ arr) = withForeignArray arr
{-# INLINE unsafeWithFArrayPtr #-}

-- A bit of unituitive trickery:
--  * `Array F ix e` isn't any different from `MArray s v ix e`, except that it is always
--    polymorphic in the element
--  * Mutable instance for F is always restricted in the element, so SIMD instructions
--    can be utilized
-- Because of the above two facts, we do the opposite from what we would normally do: we
-- freeze the mutable array in order to mutate the pointer.
-- | Access the pointer to the first element of the mutable array.
--
-- @since 0.1.0
withMFArrayPtr :: Mutable F ix e => MArray RealWorld F ix e -> (Ptr e -> IO a) -> IO a
withMFArrayPtr marr f = unsafeFreeze Seq marr >>= (`unsafeWithFArrayPtr` f)
{-# INLINE withMFArrayPtr #-}

