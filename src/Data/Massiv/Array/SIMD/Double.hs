{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Massiv.Array.SIMD.Double
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.SIMD.Double where

import Data.Massiv.Core
import Data.Coerce
import Control.Monad.Primitive
import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe
import Foreign.C
import Data.Massiv.Core.List
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (mapM)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector.Storable as VS

#include "massiv.h"


-- | Representation for SIMD vectorizable elements
data V = V deriving Show

type instance EltRepr V ix = V



data instance Array V ix e = VArray
  { vComp :: !Comp
  , vSize :: !(Sz ix)
  , vPtr  :: !(Ptr e)
  , vData :: !(ForeignPtr e)
  }

instance (Ragged L ix e, Show e, Mutable V ix e) => Show (Array V ix e) where
  showsPrec = showsArrayPrec id
  showList = showArrayList

-- instance NFData ix => NFData (Array S ix e) where
--   rnf (SArray c sz v) = c `deepseq` sz `deepseq` v `deepseq` ()
--   {-# INLINE rnf #-}

-- instance (VS.Storable e, Eq e, Index ix) => Eq (Array S ix e) where
--   (==) = eq (==)
--   {-# INLINE (==) #-}

-- instance (VS.Storable e, Ord e, Index ix) => Ord (Array S ix e) where
--   compare = ord compare
--   {-# INLINE compare #-}

instance (Index ix, Mutable V ix e) => Construct V ix e where
  setComp comp arr = arr { vComp = comp }
  {-# INLINE setComp #-}

  makeArrayLinear !comp !sz f = unsafePerformIO $ generateArrayLinear comp sz (pure . f)
  {-# INLINE makeArrayLinear #-}


-- instance (VS.Storable e, Index ix) => Source S ix e where
--   unsafeLinearIndex (SArray _ _ v) =
--     INDEX_CHECK("(Source S ix e).unsafeLinearIndex", Sz . VS.length, VS.unsafeIndex) v
--   {-# INLINE unsafeLinearIndex #-}

-- instance Index ix => Resize S ix where
--   unsafeResize !sz !arr = arr { sSize = sz }
--   {-# INLINE unsafeResize #-}

-- instance (VS.Storable e, Index ix) => Extract S ix e where
--   unsafeExtract !sIx !newSz !arr = unsafeExtract sIx newSz (toManifest arr)
--   {-# INLINE unsafeExtract #-}


instance Index ix => Load V ix Double where

  getComp (VArray comp _ _ _) = comp
  {-# INLINE getComp #-}

  size (VArray _ sz _ _) = sz
  {-# INLINE size #-}

  loadArrayM !scheduler !arr =
    splitLinearlyWith_ scheduler (elemsCount arr) (unsafeLinearIndex arr)
  {-# INLINE loadArrayM #-}

instance (Storable e, Load V ix e) => Source V ix e where
  unsafeLinearIndex (VArray _ sz p fp) = unsafeLinearForeignIndex sz p fp
  {-# INLINE unsafeLinearIndex #-}


instance Index ix => Manifest V ix Double where
  unsafeLinearIndexM (VArray _ sz p fp) = unsafeLinearForeignIndex sz p fp
  {-# INLINE unsafeLinearIndexM #-}

unsafeLinearForeignIndex ::
     Storable e => Sz ix -> Ptr e -> ForeignPtr e -> Int -> e
unsafeLinearForeignIndex sz p fp =
  INDEX_CHECK("(Source V ix e).unsafeLinearForeignIndex", sz, \ _ i -> unsafePerformIO (unsafeLinearForeignRead p fp i)) sz


unsafeLinearForeignRead :: Storable b => Ptr b -> ForeignPtr a -> Int -> IO b
unsafeLinearForeignRead p fp i = do
  e <- peek (advancePtr p i)
  touchForeignPtr fp
  pure e
{-# INLINE unsafeLinearForeignRead #-}


instance {-# OVERLAPPING #-} (Load V Ix1 e, Mutable V Ix1 e) => OuterSlice V Ix1 e where
  unsafeOuterSlice = unsafeLinearIndex
  {-# INLINE unsafeOuterSlice #-}

instance ( Index ix
         , Index (Lower ix)
         , Elt V ix e ~ Array V (Lower ix) e
         , Storable e
         , Load V ix e
         ) =>
         OuterSlice V ix e where
  unsafeOuterSlice (VArray comp sz p fp) i =
    VArray comp (snd (unconsSz sz)) (advancePtr p kStart) fp
    where
      !kStart = toLinearIndex sz (consDim i (zeroIndex :: Lower ix))
  {-# INLINE unsafeOuterSlice #-}


instance Index ix => Mutable V ix Double where
  data MArray s V ix Double = MArrayDouble !(Sz ix) !(Ptr Double) !(ForeignPtr Double)
  msize (MArrayDouble sz _ _) = sz
  {-# INLINE msize #-}
  unsafeThaw (VArray _ sz p fp) = pure $ MArrayDouble sz p fp
  {-# INLINE unsafeThaw #-}
  unsafeFreeze comp (MArrayDouble sz p fp) = pure $ VArray comp sz p fp
  {-# INLINE unsafeFreeze #-}
  unsafeNew sz = unsafePrimToPrim $ do
    p <- mallocArray (totalElem sz)
    fp <- newForeignPtr finalizerFree p
    pure $ MArrayDouble sz p fp
    --(fp, _) <- unsafeMArrayToForeignPtr <$> unsafeNew sz
    --pure $ MArrayDouble sz (unsafeForeignPtrToPtr fp) fp
  {-# INLINE unsafeNew #-}
  -- -- TODO: Use Prim fillByteArray
  initialize = undefined
  {-# INLINE initialize #-}
  unsafeLinearRead (MArrayDouble _ p fp) =
    unsafePrimToPrim . unsafeLinearForeignRead p fp
  {-# INLINE unsafeLinearRead #-}
  unsafeLinearWrite (MArrayDouble _ p fp) i e =
    unsafePrimToPrim $ do
      poke (advancePtr p i) e
      touchForeignPtr fp
  {-# INLINE unsafeLinearWrite #-}
  -- -- TODO: Use Prim setByteArray
  -- -- unsafeLinearSet (MSArray _ v) = setByteArray ma
  -- -- {-# INLINE unsafeLinearSet #-}
  unsafeLinearCopy (MArrayDouble _ ps fps) iFrom (MArrayDouble _ pd fpd) iTo (Sz k) =
    unsafePrimToPrim $ do
      let ptrFrom' = advancePtr ps iFrom
          ptrTo' = advancePtr pd iTo
      copyArray ptrTo' ptrFrom' k
      touchForeignPtr fps
      touchForeignPtr fpd
  {-# INLINE unsafeLinearCopy #-}
  -- unsafeArrayLinearCopy arrFrom iFrom marrTo iTo sz = do
  --   marrFrom <- unsafeThaw arrFrom
  --   unsafeLinearCopy marrFrom iFrom marrTo iTo sz
  -- {-# INLINE unsafeArrayLinearCopy #-}
  -- unsafeLinearShrink marr@(MSArray _ mv@(MVS.MVector _ (ForeignPtr _ fpc))) sz = do
  --   let shrinkMBA :: MutableByteArray RealWorld -> IO ()
  --       shrinkMBA mba = shrinkMutableByteArray mba (totalElem sz * sizeOf (undefined :: e))
  --       {-# INLINE shrinkMBA #-}
  --   case fpc of
  --     MallocPtr mba# _ -> do
  --       unsafePrimToPrim $ shrinkMBA (MutableByteArray mba#)
  --       pure $ MSArray sz mv
  --     PlainPtr mba# -> do
  --       unsafePrimToPrim $ shrinkMBA (MutableByteArray mba#)
  --       pure $ MSArray sz mv
  --     _ -> unsafeDefaultLinearShrink marr sz
  -- {-# INLINE unsafeLinearShrink #-}
  -- unsafeLinearGrow (MSArray _ mv) sz = MSArray sz <$> MVS.unsafeGrow mv (totalElem sz)
  -- {-# INLINE unsafeLinearGrow #-}


dotDouble :: Array V Ix1 Double -> Array V Ix1 Double -> Double
dotDouble v1@(VArray _ (Sz k1) p1 fp1) v2@(VArray _ (Sz k2) p2 fp2) =
  unsafePerformIO $ do
    let (q, r) = min k1 k2 `quotRem` 2
    e <- c_dot__m128d p1 p2 (fromIntegral (q * 2))
    touchForeignPtr fp1
    touchForeignPtr fp2
    if r == 0
      then pure e
      else let !lastIx = q * 2
            in pure
                 (e + unsafeLinearIndex v1 lastIx * unsafeLinearIndex v2 lastIx)
{-# INLINE dotDouble #-}


multiplyTransposedSIMD ::
     Array V Ix2 Double -> Array V Ix2 Double -> Array D Ix2 Double
multiplyTransposedSIMD arr1 arr2
  | n1 /= m2 = throw $ SizeMismatchException (size arr1) (size arr2)
  | otherwise =
    makeArrayR D (getComp arr1 <> getComp arr2) (SafeSz (m1 :. n2)) $ \(i :. j) ->
      -- dotDouble (unsafeOuterSlice arr1 i) (unsafeOuterSlice arr2 j)
      dotDouble (arr1 !> i) (arr2 !> j)
  where
    SafeSz (m1 :. n1) = size arr1
    SafeSz (n2 :. m2) = size arr2
{-# INLINE multiplyTransposedSIMD #-}



foreign import ccall unsafe "m128d.c dot__m128d"
  c_dot__m128d :: Ptr Double -> Ptr Double -> CLLong -> IO Double


foreign import ccall safe "m128d.c sum_d"
  c_sum_d :: Ptr Double -> Int -> IO Double

