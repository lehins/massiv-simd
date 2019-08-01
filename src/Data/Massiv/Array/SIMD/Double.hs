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
import qualified Data.Massiv.Array.SIMD.Double.M128d as SIMD
--import qualified Data.Massiv.Array.SIMD.Double.M256d as SIMD
import Data.Massiv.Array.SIMD.Internal
import Data.Massiv.Array.Unsafe
import Data.Massiv.Core.List
import Data.Massiv.Core.Operations
import Numeric
import Prelude hiding (mapM)
import System.IO.Unsafe (unsafePerformIO)


instance (Ragged L ix e, Show e, Mutable F ix e) => Show (Array F ix e) where
  showsPrec = showsArrayPrec id
  showList = showArrayList

instance NFData ix => NFData (Array F ix e) where
  rnf (VArray comp arr) = comp `deepseq` rnf arr
  {-# INLINE rnf #-}

instance Index ix => Eq (Array F ix Double) where
  (==) a1 a2 =
    size a1 == size a2 &&
    unsafePerformIO (splitReduce2 (const eqDouble) (\x y -> pure (x && y)) True a1 a2)
  -- (==) a1 a2 = size a1 == size a2 && A.and (A.zipWith (==) a1 a2)
  {-# INLINE (==) #-}

eqDouble :: Index ix => Array F ix Double -> Array F ix Double -> IO Bool
eqDouble (VArray _ arr1) (VArray _ arr2) = SIMD.eqForeignArray arr1 arr2
{-# INLINE eqDouble #-}


-- instance (VS.Storable e, Ord e, Index ix) => Ord (Array S ix e) where
--   compare = ord compare
--   {-# INLINE compare #-}

instance (Index ix, Mutable F ix e) => Construct F ix e where
  setComp comp arr = arr { vComp = comp }
  {-# INLINE setComp #-}

  makeArrayLinear !comp !sz f = unsafePerformIO $ generateArrayLinear comp sz (pure . f)
  {-# INLINE makeArrayLinear #-}


instance Index ix => Resize F ix where
  unsafeResize !sz !arr = arr {vArray = (vArray arr) {foreignArraySize = sz}}
  {-# INLINE unsafeResize #-}

instance (Load F Ix1 e, A.Storable e) => Extract F Ix1 e where
  unsafeExtract !sIx !newSz (VArray comp arr) = VArray comp (extractForeignArray sIx newSz arr)
  {-# INLINE unsafeExtract #-}


instance Index ix => Load F ix Double where

  getComp = vComp
  {-# INLINE getComp #-}

  size = foreignArraySize . vArray
  {-# INLINE size #-}

  loadArrayM !scheduler !arr =
    splitLinearlyWith_ scheduler (elemsCount arr) (unsafeLinearIndex arr)
  {-# INLINE loadArrayM #-}

instance (Storable e, Load F ix e) => Source F ix e where
  unsafeLinearIndex (VArray _ arr) = unsafeInlineIO . readForeignArray arr
  {-# INLINE unsafeLinearIndex #-}
  unsafeLinearSlice ix sz (VArray comp arr) = VArray comp $ extractForeignArray ix sz arr
  {-# INLINE unsafeLinearSlice #-}


instance (Storable e, Load F ix e) => Manifest F ix e where
  unsafeLinearIndexM (VArray _ arr) = unsafeInlineIO . readForeignArray arr
  {-# INLINE unsafeLinearIndexM #-}


instance {-# OVERLAPPING #-} (Storable e, Load F Ix1 e) => OuterSlice F Ix1 e where
  unsafeOuterSlice = unsafeLinearIndex
  {-# INLINE unsafeOuterSlice #-}

instance ( Index ix
         , Index (Lower ix)
         , Elt F ix e ~ Array F (Lower ix) e
         , Storable e
         , Load F ix e
         ) =>
         OuterSlice F ix e where
  unsafeOuterSlice (VArray comp arr) = VArray comp . sliceForeignArray arr
  {-# INLINE unsafeOuterSlice #-}


instance Index ix => Mutable F ix Double where
  newtype MArray s F ix Double = MArrayDouble (ForeignArray ix Double)
  msize (MArrayDouble arr) = foreignArraySize arr
  {-# INLINE msize #-}
  unsafeThaw (VArray _ arr) = pure (MArrayDouble arr)
  {-# INLINE unsafeThaw #-}
  unsafeFreeze comp (MArrayDouble arr) = pure $ VArray comp arr
  {-# INLINE unsafeFreeze #-}
  unsafeNew sz = unsafePrimToPrim (MArrayDouble <$> mallocForeignArray sz)
  {-# INLINE unsafeNew #-}
  initialize (MArrayDouble arr) = unsafePrimToPrim $ SIMD.fillForeignArray 0 arr
  {-# INLINE initialize #-}
  unsafeLinearRead (MArrayDouble arr) = unsafePrimToPrim . readForeignArray arr
  {-# INLINE unsafeLinearRead #-}
  unsafeLinearWrite (MArrayDouble arr) i = unsafePrimToPrim . writeForeignArray arr i
  {-# INLINE unsafeLinearWrite #-}
  unsafeLinearSet (MArrayDouble arr) offset len val =
    unsafePrimToPrim $ SIMD.fillForeignArray val (extractForeignArray offset len arr)
  {-# INLINE unsafeLinearSet #-}
  unsafeLinearCopy (MArrayDouble arrs) iFrom (MArrayDouble arrd) iTo k =
    unsafePrimToPrim $
    SIMD.copyForeignArray (extractForeignArray iFrom k arrs) (extractForeignArray iTo k arrd)
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


splitApply ::
     (Mutable F ix e1, Storable e1, Storable e2)
  => (ForeignArray Ix1 e2 -> ForeignArray Ix1 e1 -> IO ())
  -> Array F ix e2
  -> Array F ix e1
splitApply f (VArray comp arr) =
  unsafePerformIO $ do
    let !sz = foreignArraySize arr
        !totalLength = totalElem sz
    marr <- unsafeNew sz
    VArray _ resArr <- unsafeFreeze Seq marr
    withScheduler_ comp $ \scheduler -> do
      let schedule chunkStart chunkLength =
            let chunk = extractForeignArray chunkStart chunkLength arr
                resChunk = extractForeignArray chunkStart chunkLength resArr
             in scheduleWork_ scheduler $ f chunk resChunk
          {-# INLINE schedule #-}
      splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
        loopM_ 0 (< slackStart) (+ chunkLength) (`schedule` SafeSz chunkLength)
        when (slackStart < totalLength) $ schedule slackStart (SafeSz (totalLength - slackStart))
    unsafeFreeze comp marr
{-# INLINE splitApply #-}


unsafeSplitApply2 ::
     (Mutable F ix e1, Storable e1, Storable e2, Storable e3)
  => (ForeignArray Ix1 e3 -> ForeignArray Ix1 e2 -> ForeignArray Ix1 e1 -> IO ())
  -> Array F ix e3
  -> Array F ix e2
  -> Array F ix e1
unsafeSplitApply2 f (VArray comp1 arr1) (VArray comp2 arr2) =
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
{-# INLINE unsafeSplitApply2 #-}

applySameSizeArray2 ::
     Load r ix e => (Array r ix e -> Array r ix e -> a) -> Array r ix e -> Array r ix e -> a
applySameSizeArray2 f a1 a2
  | size a1 == size a2 = f a1 a2
  | otherwise = throw $ SizeMismatchException (size a1) (size a2)
{-# INLINE applySameSizeArray2 #-}


instance ReduceNumeric F Double where
  sumArrayS (VArray _ arr) = unsafeInlineIO $ SIMD.sumForeignArray arr
  {-# INLINE sumArrayS #-}
  productArrayS (VArray _ arr) = unsafeInlineIO $ SIMD.productForeignArray arr
  {-# INLINE productArrayS #-}
  evenPowerSumArrayS (VArray _ arr) = unsafeInlineIO . SIMD.evenPowerSumForeignArray arr
  {-# INLINE evenPowerSumArrayS #-}
  absPowerSumArrayS (VArray _ arr) = unsafeInlineIO . SIMD.absPowerSumForeignArray arr
  {-# INLINE absPowerSumArrayS #-}
  multiplySumArrayS (VArray _ arr1) (VArray _ arr2) =
    unsafeInlineIO $ SIMD.multiplySumForeignArray arr1 arr2
  {-# INLINE multiplySumArrayS #-}


instance Numeric F Double where
  plusScalar arr x = splitApply (`SIMD.plusScalarForeignArray` x) arr
  {-# INLINE plusScalar #-}
  minusScalar arr x = splitApply (`SIMD.minusScalarForeignArray` x) arr
  {-# INLINE minusScalar #-}
  multiplyScalar arr x = splitApply (`SIMD.multiplyScalarForeignArray` x) arr
  {-# INLINE multiplyScalar #-}
  absPointwise = splitApply SIMD.absForeignArray
  {-# INLINE absPointwise #-}
  additionPointwise = unsafeSplitApply2 SIMD.additionForeignArray
  {-# INLINE additionPointwise #-}
  subtractionPointwise = unsafeSplitApply2 SIMD.subtractionForeignArray
  {-# INLINE subtractionPointwise #-}
  multiplicationPointwise = unsafeSplitApply2 SIMD.multiplicationForeignArray
  {-# INLINE multiplicationPointwise #-}
  unsafeLiftArray f a = makeArrayLinear (vComp a) (size a) (f . unsafeLinearIndex a)
  {-# INLINE unsafeLiftArray #-}
  unsafeLiftArray2 f a1 a2 =
    makeArrayLinear (vComp a1 <> vComp a2) (size a1) $ \ !i ->
      f (unsafeLinearIndex a1 i) (unsafeLinearIndex a2 i)
  {-# INLINE unsafeLiftArray2 #-}

instance NumericFloat F Double where
  divideScalar arr x = splitApply (`SIMD.divideScalarForeignArray` x) arr
  {-# INLINE divideScalar #-}
  recipMultiplyScalar arr x = splitApply (`SIMD.recipMultiplyForeignArray` x) arr
  {-# INLINE recipMultiplyScalar #-}
  divisionPointwise = unsafeSplitApply2 SIMD.divisionForeignArray
  {-# INLINE divisionPointwise #-}
  recipPointwise = splitApply (`SIMD.recipMultiplyForeignArray` 1)
  {-# INLINE recipPointwise #-}
  sqrtPointwise = splitApply SIMD.sqrtForeignArray
  {-# INLINE sqrtPointwise #-}

instance (Numeric F e, Mutable F ix e, Storable e) => Num (Array F ix e) where
  (+) = applySameSizeArray2 additionPointwise
  {-# INLINE (+) #-}
  (-) = applySameSizeArray2 subtractionPointwise
  {-# INLINE (-) #-}
  (*) = applySameSizeArray2 multiplicationPointwise
  {-# INLINE (*) #-}
  abs = absPointwise
  {-# INLINE abs #-}
  signum = A.compute . A.map signum
  {-# INLINE signum #-}
  fromInteger = singleton . fromInteger
  {-# INLINE fromInteger #-}

instance (NumericFloat F e, Mutable F ix e, Storable e) => Fractional (Array F ix e) where
  (/) = applySameSizeArray2 divisionPointwise
  {-# INLINE (/) #-}
  recip = recipPointwise
  {-# INLINE recip #-}
  fromRational = singleton . fromRational
  {-# INLINE fromRational #-}

instance (NumericFloat F e, Mutable F ix e, Storable e) => Floating (Array F ix e) where
  pi    = singleton pi
  {-# INLINE pi #-}
  exp   = unsafeLiftArray exp
  {-# INLINE exp #-}
  log   = unsafeLiftArray log
  {-# INLINE log #-}
  sin   = unsafeLiftArray sin
  {-# INLINE sin #-}
  cos   = unsafeLiftArray cos
  {-# INLINE cos #-}
  asin  = unsafeLiftArray asin
  {-# INLINE asin #-}
  atan  = unsafeLiftArray atan
  {-# INLINE atan #-}
  acos  = unsafeLiftArray acos
  {-# INLINE acos #-}
  sinh  = unsafeLiftArray sinh
  {-# INLINE sinh #-}
  cosh  = unsafeLiftArray cosh
  {-# INLINE cosh #-}
  asinh = unsafeLiftArray asinh
  {-# INLINE asinh #-}
  atanh = unsafeLiftArray atanh
  {-# INLINE atanh #-}
  acosh = unsafeLiftArray acosh
  {-# INLINE acosh #-}

  -- Override default implementation
  sqrt = sqrtPointwise
  {-# INLINE sqrt #-}
  (**) = applySameSizeArray2 (unsafeLiftArray2 (**))
  {-# INLINE (**) #-}
  tan = unsafeLiftArray tan
  {-# INLINE tan #-}
  tanh = unsafeLiftArray tanh
  {-# INLINE tanh #-}
  log1p = unsafeLiftArray log1p
  {-# INLINE log1p #-}
  expm1 = unsafeLiftArray expm1
  {-# INLINE expm1 #-}
