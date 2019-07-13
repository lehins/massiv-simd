-- |
-- Module      : Data.Massiv.Array.SIMD
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.SIMD
  ( module Data.Massiv.Array.SIMD.Internal
  , module Data.Massiv.Array.SIMD.Double
  ) where

import Data.Massiv.Array.SIMD.Internal
import Data.Massiv.Array.SIMD.Double
-- import Data.Coerce
-- import Control.DeepSeq
-- import Control.Monad.Primitive
-- import Data.Massiv.Array as A
-- import Data.Massiv.Array.ForeignArray
-- import Data.Massiv.Array.SIMD.Double
-- import Data.Massiv.Array.Unsafe
-- import Data.Massiv.Core.List
-- import Foreign.C
-- import Foreign.ForeignPtr
-- import Foreign.Marshal.Array
-- import Foreign.Ptr
-- import Prelude hiding (mapM)
-- import System.IO.Unsafe (unsafePerformIO)

-- #include "massiv.h"


-- -- | Representation for arrays with SIMD vectorizable elements
-- data V = V deriving Show

-- type instance EltRepr V ix = V



-- data instance Array V ix e = VArray
--   { vComp :: !Comp
--   , vArray :: !(ForeignArray ix e)
--   }

-- instance (Ragged L ix e, Show e, Mutable V ix e) => Show (Array V ix e) where
--   showsPrec = showsArrayPrec id
--   showList = showArrayList

-- instance NFData ix => NFData (Array V ix e) where
--   rnf (VArray comp arr) = comp `deepseq` rnf arr
--   {-# INLINE rnf #-}

-- instance (Mutable V ix e, Eq e, Index ix) => Eq (Array V ix e) where
--   (==) a1 a2 = A.and $ A.zipWith (==) a1 a2
--   {-# INLINE (==) #-}

-- -- instance (VS.Storable e, Ord e, Index ix) => Ord (Array S ix e) where
-- --   compare = ord compare
-- --   {-# INLINE compare #-}

-- instance (Index ix, Mutable V ix e) => Construct V ix e where
--   setComp comp arr = arr { vComp = comp }
--   {-# INLINE setComp #-}

--   makeArrayLinear !comp !sz f = unsafePerformIO $ generateArrayLinear comp sz (pure . f)
--   {-# INLINE makeArrayLinear #-}


-- instance Index ix => Resize V ix where
--   unsafeResize !sz !arr = arr {vArray = (vArray arr) {foreignArraySize = sz}}
--   {-# INLINE unsafeResize #-}

-- instance (Load V Ix1 e, A.Storable e) => Extract V Ix1 e where
--   unsafeExtract !sIx !newSz (VArray comp arr) = VArray comp (extractForeignArray sIx newSz arr)
--   {-# INLINE unsafeExtract #-}


-- instance Index ix => Load V ix Double where

--   getComp = vComp
--   {-# INLINE getComp #-}

--   size = foreignArraySize . vArray
--   {-# INLINE size #-}

--   loadArrayM !scheduler !arr =
--     splitLinearlyWith_ scheduler (elemsCount arr) (unsafeLinearIndex arr)
--   {-# INLINE loadArrayM #-}

-- instance (Storable e, Load V ix e) => Source V ix e where
--   unsafeLinearIndex (VArray _ arr) = unsafePerformIO . readForeignArray arr
--   {-# INLINE unsafeLinearIndex #-}


-- instance (Storable e, Load V ix e) => Manifest V ix e where
--   unsafeLinearIndexM (VArray _ arr) = unsafePerformIO . readForeignArray arr
--   {-# INLINE unsafeLinearIndexM #-}


-- instance {-# OVERLAPPING #-} (Storable e, Load V Ix1 e) => OuterSlice V Ix1 e where
--   unsafeOuterSlice = unsafeLinearIndex
--   {-# INLINE unsafeOuterSlice #-}

-- instance ( Index ix
--          , Index (Lower ix)
--          , Elt V ix e ~ Array V (Lower ix) e
--          , Storable e
--          , Load V ix e
--          ) =>
--          OuterSlice V ix e where
--   unsafeOuterSlice (VArray comp arr) = VArray comp . sliceForeignArray arr
--   {-# INLINE unsafeOuterSlice #-}


-- instance Index ix => Mutable V ix Double where
--   newtype MArray s V ix Double = MArrayDouble (ForeignArray ix
--                                                Double)
--   msize (MArrayDouble arr) = foreignArraySize arr
--   {-# INLINE msize #-}
--   unsafeThaw (VArray _ arr) = pure (MArrayDouble arr)
--   {-# INLINE unsafeThaw #-}
--   unsafeFreeze comp (MArrayDouble arr) = pure $ VArray comp arr
--   {-# INLINE unsafeFreeze #-}
--   unsafeNew sz = unsafePrimToPrim (MArrayDouble <$> mallocForeignArray sz)
--   {-# INLINE unsafeNew #-}
--   -- -- TODO: Use Prim fillByteArray
--   initialize (MArrayDouble arr) =
--     unsafePrimToPrim $ broadcastDouble arr 0 (sizeForeignArray arr) 0
--   {-# INLINE initialize #-}
--   initializeNew mDef sz =
--     case mDef of
--       Nothing -> unsafePrimToPrim (MArrayDouble <$> callocForeignArray sz)
--       Just defVal -> do
--         marr <- unsafeNew sz
--         unsafeLinearSet marr 0 (totalElem sz) defVal
--         pure marr
--   {-# INLINE initializeNew #-}
--   unsafeLinearRead (MArrayDouble arr) = unsafePrimToPrim . readForeignArray arr
--   {-# INLINE unsafeLinearRead #-}
--   unsafeLinearWrite (MArrayDouble arr) i =
--     unsafePrimToPrim . writeForeignArray arr i
--   {-# INLINE unsafeLinearWrite #-}
--   unsafeLinearSet (MArrayDouble arr) offset len =
--     unsafePrimToPrim . broadcastDouble arr offset (Sz len)
--   {-# INLINE unsafeLinearSet #-}
--   unsafeLinearCopy (MArrayDouble arrs) iFrom (MArrayDouble arrd) iTo =
--     unsafePrimToPrim . copyDouble arrs iFrom arrd iTo
--   {-# INLINE unsafeLinearCopy #-}
--   unsafeArrayLinearCopy arrFrom iFrom marrTo iTo sz = do
--     marrFrom <- unsafeThaw arrFrom
--     unsafeLinearCopy marrFrom iFrom marrTo iTo sz
--   {-# INLINE unsafeArrayLinearCopy #-}
--   -- unsafeLinearShrink (MArrayDouble _ freed curPtr fp) sz =
--   --   unsafePrimToPrim $
--   --   withForeignPtr fp $ \ptr -> do
--   --     let offset = (curPtr `minusPtr` ptr) `div` sizeOf (undefined :: Double)
--   --     rPtr <- reallocArray ptr (offset + totalElem sz)
--   --     if rPtr == ptr
--   --       then pure (MArrayDouble sz freed curPtr fp)
--   --       else do
--   --         fp' <- newForeignPtrEnv finalizerFreeFlagged freed rPtr
--   --         pure $ MArrayDouble sz freed (advancePtr rPtr offset) fp'
--   -- unsafeLinearGrow (MArrayDouble _ freed curPtr fp) =
--   --   unsafePrimToPrim . unsafeReallocMVArrayDouble freed curPtr fp
--   -- {-# INLINE unsafeLinearGrow #-}


-- -- | Access the pointer to the first element of the array. It is unsafe to mutate the
-- -- pointer, unless no one else is holding a reference to this array, or any other
-- -- parrent array if that one was a result of a slice.
-- --
-- -- @since 0.1.0
-- unsafeWithVArrayPtr :: Array V ix e -> (Ptr e -> IO a) -> IO a
-- unsafeWithVArrayPtr (VArray _ arr) = withForeignArray arr
-- {-# INLINE unsafeWithVArrayPtr #-}

-- -- A bit of unituitive trickery:
-- --  * `Array V ix e` isn't any different from `MArray s v ix e`, except that it is always
-- --    polymorphic in the element
-- --  * Mutable instance for V is always restricted in the element, so SIMD instructions
-- --    can be utilized
-- -- Because of the above two facts, we do the opposite from what we would normally do: we
-- -- freeze the mutable array in order to mutate the pointer.
-- -- | Access the pointer to the first element of the mutable array.
-- --
-- -- @since 0.1.0
-- withMVArrayPtr :: Mutable V ix e => MArray RealWorld V ix e -> (Ptr e -> IO a) -> IO a
-- withMVArrayPtr marr f = unsafeFreeze Seq marr >>= (`unsafeWithVArrayPtr` f)
-- {-# INLINE withMVArrayPtr #-}
