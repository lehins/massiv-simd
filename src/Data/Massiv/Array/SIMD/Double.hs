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

import Control.DeepSeq
import Control.Monad (when)
import Control.Monad.Primitive
import Control.Scheduler
import Data.Massiv.Array as A
import Data.Massiv.Array.ForeignArray
import Data.Massiv.Array.SIMD.Internal
import Data.Massiv.Array.Unsafe
import Data.Massiv.Core.List
import Data.Massiv.Core.Operations
import qualified Data.Massiv.Array.SIMD.Double.M128d as SIMD
--import qualified Data.Massiv.Array.SIMD.Double.M256d as SIMD
import Prelude hiding (mapM)
import System.IO.Unsafe (unsafePerformIO)


instance (Ragged L ix e, Show e, Mutable V ix e) => Show (Array V ix e) where
  showsPrec = showsArrayPrec id
  showList = showArrayList

instance NFData ix => NFData (Array V ix e) where
  rnf (VArray comp arr) = comp `deepseq` rnf arr
  {-# INLINE rnf #-}

instance Index ix => Eq (Array V ix Double) where
  (==) = eqDouble
  {-# INLINE (==) #-}

-- instance (VS.Storable e, Ord e, Index ix) => Ord (Array S ix e) where
--   compare = ord compare
--   {-# INLINE compare #-}

instance (Index ix, Mutable V ix e) => Construct V ix e where
  setComp comp arr = arr { vComp = comp }
  {-# INLINE setComp #-}

  makeArrayLinear !comp !sz f = unsafePerformIO $ generateArrayLinear comp sz (pure . f)
  {-# INLINE makeArrayLinear #-}


instance Index ix => Resize V ix where
  unsafeResize !sz !arr = arr {vArray = (vArray arr) {foreignArraySize = sz}}
  {-# INLINE unsafeResize #-}

instance (Load V Ix1 e, A.Storable e) => Extract V Ix1 e where
  unsafeExtract !sIx !newSz (VArray comp arr) = VArray comp (extractForeignArray sIx newSz arr)
  {-# INLINE unsafeExtract #-}


instance Index ix => Load V ix Double where

  getComp = vComp
  {-# INLINE getComp #-}

  size = foreignArraySize . vArray
  {-# INLINE size #-}

  loadArrayM !scheduler !arr =
    splitLinearlyWith_ scheduler (elemsCount arr) (unsafeLinearIndex arr)
  {-# INLINE loadArrayM #-}

instance (Storable e, Load V ix e) => Source V ix e where
  unsafeLinearIndex (VArray _ arr) = unsafeInlineIO . readForeignArray arr
  {-# INLINE unsafeLinearIndex #-}


instance (Storable e, Load V ix e) => Manifest V ix e where
  unsafeLinearIndexM (VArray _ arr) = unsafeInlineIO . readForeignArray arr
  {-# INLINE unsafeLinearIndexM #-}


instance {-# OVERLAPPING #-} (Storable e, Load V Ix1 e) => OuterSlice V Ix1 e where
  unsafeOuterSlice = unsafeLinearIndex
  {-# INLINE unsafeOuterSlice #-}

instance ( Index ix
         , Index (Lower ix)
         , Elt V ix e ~ Array V (Lower ix) e
         , Storable e
         , Load V ix e
         ) =>
         OuterSlice V ix e where
  unsafeOuterSlice (VArray comp arr) = VArray comp . sliceForeignArray arr
  {-# INLINE unsafeOuterSlice #-}


instance Index ix => Mutable V ix Double where
  newtype MArray s V ix Double = MArrayDouble (ForeignArray ix
                                               Double)
  msize (MArrayDouble arr) = foreignArraySize arr
  {-# INLINE msize #-}
  unsafeThaw (VArray _ arr) = pure (MArrayDouble arr)
  {-# INLINE unsafeThaw #-}
  unsafeFreeze comp (MArrayDouble arr) = pure $ VArray comp arr
  {-# INLINE unsafeFreeze #-}
  unsafeNew sz = unsafePrimToPrim (MArrayDouble <$> newForeignArray sz)
  {-# INLINE unsafeNew #-}
  initialize (MArrayDouble arr) =
    unsafePrimToPrim $ SIMD.broadcastDouble arr 0 (sizeForeignArray arr) 0
  {-# INLINE initialize #-}
  unsafeLinearRead (MArrayDouble arr) = unsafePrimToPrim . readForeignArray arr
  {-# INLINE unsafeLinearRead #-}
  unsafeLinearWrite (MArrayDouble arr) i = unsafePrimToPrim . writeForeignArray arr i
  {-# INLINE unsafeLinearWrite #-}
  unsafeLinearSet (MArrayDouble arr) offset len =
    unsafePrimToPrim . SIMD.broadcastDouble arr offset (Sz len)
  {-# INLINE unsafeLinearSet #-}
  unsafeLinearCopy (MArrayDouble arrs) iFrom (MArrayDouble arrd) iTo =
    unsafePrimToPrim . SIMD.copyDouble arrs iFrom arrd iTo
  {-# INLINE unsafeLinearCopy #-}
  unsafeArrayLinearCopy arrFrom iFrom marrTo iTo sz = do
    marrFrom <- unsafeThaw arrFrom
    unsafeLinearCopy marrFrom iFrom marrTo iTo sz
  {-# INLINE unsafeArrayLinearCopy #-}
  unsafeLinearShrink (MArrayDouble arr) =
    fmap MArrayDouble . unsafePrimToPrim . reallocForeignArray arr
  {-# INLINE unsafeLinearShrink #-}
  unsafeLinearGrow (MArrayDouble arr) =
    fmap MArrayDouble . unsafePrimToPrim . reallocForeignArray arr
  {-# INLINE unsafeLinearGrow #-}

dotDouble :: Index ix => Array V ix Double -> Array V ix Double -> Double
dotDouble (VArray _ arr1) (VArray _ arr2) = unsafePerformIO $ SIMD.dotDouble arr1 arr2
{-# INLINE dotDouble #-}

eqDouble :: Index ix => Array V ix Double -> Array V ix Double -> Bool
eqDouble (VArray _ arr1) (VArray _ arr2) =
  unsafePerformIO $ SIMD.eqDouble arr1 arr2
{-# INLINE eqDouble #-}


multiplySIMD
  :: Source r Ix2 Double =>
     Array V Ix2 Double -> Array r Ix2 Double -> Array D Ix2 Double
multiplySIMD arr1 arr2 = multiplyTransposedSIMD arr1 arr2'
  where
    arr2' = compute $ transpose arr2
{-# INLINE multiplySIMD #-}


multiplyTransposedSIMD ::
     Array V Ix2 Double -> Array V Ix2 Double -> Array D Ix2 Double
multiplyTransposedSIMD arr1 arr2
  | n1 /= m2 = throw $ SizeMismatchException (size arr1) (size arr2)
  | otherwise =
    makeArrayR D (getComp arr1 <> getComp arr2) (SafeSz (m1 :. n2)) $ \(i :. j) ->
      dotDouble (unsafeOuterSlice arr1 i) (unsafeOuterSlice arr2 j)
  where
    SafeSz (m1 :. n1) = size arr1
    SafeSz (n2 :. m2) = size arr2
{-# INLINE multiplyTransposedSIMD #-}

sumDouble :: Index ix => Array V ix Double -> Double
sumDouble (VArray _ arr) = unsafePerformIO $ SIMD.sumDouble arr
{-# INLINE sumDouble #-}

productDouble :: Index ix => Array V ix Double -> Double
productDouble (VArray _ arr) = unsafePerformIO $ SIMD.productDouble arr
{-# INLINE productDouble #-}

maximumDouble :: Index ix => Array V ix Double -> Double
maximumDouble (VArray _ arr) = unsafePerformIO $ SIMD.maximumDouble arr
{-# INLINE maximumDouble #-}



-- copyDouble :: MArray V ix1 Double -> Ix1 -> Array V ix2 Double -> Ix1 -> Sz1 -> IO ()
-- copyDouble (MVArray arrs) iFrom (VArray _ arrd) iTo k =
--   unsafePerformIO $ SIMD.copyDouble arrs iFrom arrd iTo k
-- {-# INLINE copyDouble #-}

-- broadcastDouble :: MArray V ix Double -> Ix1 -> Sz1 -> Double -> IO ()
-- broadcastDouble (VArray _ arr) offset k e =
--     unsafePerformIO $ SIMD.broadcastDouble arr offset k e
-- {-# INLINE broadcastDouble #-}


splitApply2 ::
     (Mutable V ix e1, Storable e1, Storable e2, Storable e3)
  => (ForeignArray Ix1 e3 -> ForeignArray Ix1 e2 -> ForeignArray Ix1 e1 -> IO ())
  -> Array V ix e3
  -> Array V ix e2
  -> Array V ix e1
splitApply2 f (VArray comp1 arr1) (VArray comp2 arr2) =
  unsafePerformIO $ do
    let !sz = foreignArraySize arr1
        !comp = comp1 <> comp2
        !totalLength = totalElem sz
    marr <- unsafeNew sz
    VArray _ resArr <- unsafeFreeze Seq marr
    withScheduler_ comp $ \scheduler -> do
      let schedule chunkStart chunkLength =
            let chunk1 = extractForeignArray chunkStart chunkLength arr1
                chunk2 = extractForeignArray chunkStart chunkLength arr2
                resChunk = extractForeignArray chunkStart chunkLength resArr
             in scheduleWork_ scheduler $ f chunk1 chunk2 resChunk
          {-# INLINE schedule #-}
      splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
        loopM_ 0 (< slackStart) (+ chunkLength) (`schedule` SafeSz chunkLength)
        when (slackStart < totalLength) $ schedule slackStart (SafeSz (totalLength - slackStart))
    unsafeFreeze comp marr
{-# INLINE splitApply2 #-}



instance Numeric V Double where
  sumArray _ = sumDouble
  {-# INLINE sumArray #-}
  productArray _ = productDouble
  {-# INLINE productArray #-}
  dotProduct _ = dotDouble
  {-# INLINE dotProduct #-}
  -- minusElementArray arr e = liftDArray (subtract e) arr
  -- {-# INLINE minusElementArray #-}
  -- multiplyElementArray arr e = liftDArray (* e) arr
  -- {-# INLINE multiplyElementArray #-}
  -- absPointwise = liftDArray abs
  -- {-# INLINE absPointwise #-}
  additionPointwise = splitApply2 SIMD.plusDouble
  {-# INLINE additionPointwise #-}


instance (Numeric V e, Mutable V ix e, Storable e) => Num (Array V ix e) where
  (+) = additionPointwise
  {-# INLINE (+) #-}
  (-) = subtractionPointwise
  {-# INLINE (-) #-}
  (*) = multiplicationPointwise
  {-# INLINE (*) #-}
  abs = absPointwise
  {-# INLINE abs #-}
  signum = A.compute . A.map signum
  {-# INLINE signum #-}
  fromInteger = singleton . fromInteger
  {-# INLINE fromInteger #-}
