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

linearSize :: Load r ix e => Array r ix e -> Sz1
linearSize = SafeSz . totalElem . size

totalSize :: Index ix => Sz ix -> Sz1
totalSize = SafeSz . totalElem

instance (Ragged L ix e, Show e, Mutable F ix e) => Show (Array F ix e) where
  showsPrec = showsArrayPrec id
  showList = showArrayList

instance NFData ix => NFData (Array F ix e) where
  rnf (FArray comp sz arr) = comp `deepseq` sz `deepseq` rnf arr
  {-# INLINE rnf #-}

instance (Storable e, Eq e, Index ix) => Eq (Array F ix e) where
  (==) = eqArrays (==)
  -- (==) a1 a2 =
  --   size a1 == size a2 &&
  --   unsafePerformIO (splitReduce2 (const eqDouble) (\x y -> pure (x && y)) True a1 a2)
  -- (==) a1 a2 = size a1 == size a2 && A.and (A.zipWith (==) a1 a2)
  {-# INLINE (==) #-}

eqDouble :: Index ix => Array F ix Double -> Array F ix Double -> IO Bool
eqDouble (FArray _ sz1 arr1) (FArray _ _ arr2) = SIMD.eqForeignArray (totalSize sz1) arr1 arr2
{-# INLINE eqDouble #-}

instance (Storable e, Ord e, Index ix) => Ord (Array F ix e) where
  compare = compareArrays compare
  {-# INLINE compare #-}

instance (Index ix, Mutable F ix e) => Construct F ix e where
  setComp comp arr = arr { vComp = comp }
  {-# INLINE setComp #-}

  makeArrayLinear !comp !sz f = unsafePerformIO $ generateArrayLinear comp sz (pure . f)
  {-# INLINE makeArrayLinear #-}

  -- makeConstantArray sz e = runST $ unsafeFreeze Seq =<< initializeNew (Just e) sz
  -- {-# INLINE makeConstantArray #-}


instance Index ix => Resize F ix where
  unsafeResize !sz !arr = arr {vSize = sz}
  {-# INLINE unsafeResize #-}

instance (Load F Ix1 e, A.Storable e) => Extract F Ix1 e where
  unsafeExtract !sIx !newSz (FArray comp _ arr) = FArray comp newSz (extractForeignArray sIx arr)
  {-# INLINE unsafeExtract #-}


instance (Storable e, Index ix) => Load F ix e where

  getComp = vComp
  {-# INLINE getComp #-}

  size = vSize
  {-# INLINE size #-}

  loadArrayM !scheduler !arr =
    splitLinearlyWith_ scheduler (elemsCount arr) (unsafeLinearIndex arr)
  {-# INLINE loadArrayM #-}

instance (Storable e, Load F ix e) => Source F ix e where
  unsafeLinearIndex (FArray _ sz arr) = unsafeInlineIO . readForeignArray (totalSize sz) arr
  {-# INLINE unsafeLinearIndex #-}
  unsafeLinearSlice ix sz (FArray comp _ arr) = FArray comp sz $ extractForeignArray ix arr
  {-# INLINE unsafeLinearSlice #-}


instance (Storable e, Load F ix e) => Manifest F ix e where
  unsafeLinearIndexM (FArray _ sz arr) = unsafeInlineIO . readForeignArray (totalSize sz) arr
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
  unsafeOuterSlice (FArray comp sz arr) i =
    FArray comp (snd (unconsSz sz)) $ extractForeignArray offset arr
    where !offset = toLinearIndex sz (consDim i (zeroIndex :: Lower ix))
  {-# INLINE unsafeOuterSlice #-}

-- instance Index ix => Mutable F ix Int64 where
--   newtype MArray s F ix Int64 = MArrayI64 (ForeignArray ix Int64)
--   msize (MArrayI64 arr) = foreignArraySize arr
--   {-# INLINE msize #-}
--   unsafeThaw (FArray _ arr) = pure (MArrayI64 arr)
--   {-# INLINE unsafeThaw #-}
--   unsafeFreeze comp (MArrayI64 arr) = pure $ FArray comp arr
--   {-# INLINE unsafeFreeze #-}
--   unsafeNew sz = unsafePrimToPrim (MArrayI64 <$> mallocForeignArray sz)
--   {-# INLINE unsafeNew #-}
--   -- initialize (MArrayI64 arr) = unsafePrimToPrim $ SIMD.fillForeignArray 0 arr
--   -- {-# INLINE initialize #-}
--   unsafeLinearRead (MArrayI64 arr) = unsafePrimToPrim . readForeignArray arr
--   {-# INLINE unsafeLinearRead #-}
--   unsafeLinearWrite (MArrayI64 arr) i = unsafePrimToPrim . writeForeignArray arr i
--   {-# INLINE unsafeLinearWrite #-}


instance Index ix => Mutable F ix Double where
  data MArray s F ix Double = MArrayD !(Sz ix) (ForeignArray Double)
  msize (MArrayD sz _) = sz
  {-# INLINE msize #-}
  -- unsafeMutableSlice ix sz (MArrayD _ arr) = MArrayD sz $ extractForeignArray ix arr
  -- {-# INLINE unsafeMutableSlice #-}
  unsafeThaw (FArray _ sz arr) = pure (MArrayD sz arr)
  {-# INLINE unsafeThaw #-}
  unsafeFreeze comp (MArrayD sz arr) = pure $ FArray comp sz arr
  {-# INLINE unsafeFreeze #-}
  unsafeNew sz = unsafePrimToPrim (MArrayD sz <$> newForeignArray (totalSize sz))
  {-# INLINE unsafeNew #-}
  initialize (MArrayD sz arr) = unsafePrimToPrim $ fillForeignArray (totalSize sz) arr 0
  {-# INLINE initialize #-}
  initializeNew mdef sz =
    case mdef of
      Just val -> do
        marr@(MArrayD _ farr) <- unsafeNew sz
        unsafePrimToPrim $ SIMD.fillForeignArray (totalSize sz) farr val
        return marr
      Nothing -> do
        marr@(MArrayD _ farr) <- unsafeNew sz
        unsafePrimToPrim $ fillForeignArray (totalSize sz) farr 0
        return marr
  {-# INLINE initializeNew #-}
  unsafeLinearRead (MArrayD sz arr) = unsafePrimToPrim . readForeignArray (totalSize sz) arr
  {-# INLINE unsafeLinearRead #-}
  unsafeLinearWrite (MArrayD sz arr) i = unsafePrimToPrim . writeForeignArray (totalSize sz) arr i
  {-# INLINE unsafeLinearWrite #-}
  unsafeLinearSet (MArrayD _ arr) offset len val =
    unsafePrimToPrim $ SIMD.fillForeignArray len (extractForeignArray offset arr) val
  {-# INLINE unsafeLinearSet #-}
  unsafeLinearCopy (MArrayD _ arrSrc) iFrom (MArrayD _ arrDst) iTo k =
    unsafePrimToPrim $
    copyForeignArray k (extractForeignArray iFrom arrSrc) (extractForeignArray iTo arrDst)
  {-# INLINE unsafeLinearCopy #-}
  unsafeArrayLinearCopy arrFrom iFrom marrTo iTo sz = do
    marrFrom <- unsafeThaw arrFrom
    unsafeLinearCopy marrFrom iFrom marrTo iTo sz
  {-# INLINE unsafeArrayLinearCopy #-}
  unsafeLinearShrink (MArrayD oldsz arr) newsz =
    MArrayD newsz <$> unsafePrimToPrim (reallocForeignArray (totalSize oldsz) arr (totalSize newsz))
  {-# INLINE unsafeLinearShrink #-}
  unsafeLinearGrow (MArrayD oldsz arr) newsz =
    MArrayD newsz <$> unsafePrimToPrim (reallocForeignArray (totalSize oldsz) arr (totalSize newsz))
  {-# INLINE unsafeLinearGrow #-}


splitApply ::
     (Mutable F ix e1, Storable e1, Storable e2)
  => (Sz1 -> ForeignArray e2 -> ForeignArray e1 -> IO ())
  -> Array F ix e2
  -> Array F ix e1
splitApply f (FArray comp sz arr) =
  unsafePerformIO $ do
    let !totalLength = totalElem sz
    marr <- unsafeNew sz
    FArray _ _ resArr <- unsafeFreeze Seq marr
    withScheduler_ comp $ \scheduler -> do
      let schedule chunkStart chunkLength =
            let chunk = extractForeignArray chunkStart arr
                resChunk = extractForeignArray chunkStart resArr
             in scheduleWork_ scheduler $ f chunkLength chunk resChunk
          {-# INLINE schedule #-}
      splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
        loopM_ 0 (< slackStart) (+ chunkLength) (`schedule` SafeSz chunkLength)
        when (slackStart < totalLength) $ schedule slackStart (SafeSz (totalLength - slackStart))
    unsafeFreeze comp marr
{-# INLINE splitApply #-}


unsafeSplitApply2 ::
     (Mutable F ix e1, Storable e1, Storable e2, Storable e3)
  => (Sz1 -> ForeignArray e3 -> ForeignArray e2 -> ForeignArray e1 -> IO ())
  -> Array F ix e3
  -> Array F ix e2
  -> Array F ix e1
unsafeSplitApply2 f (FArray comp1 sz arr1) (FArray comp2 _ arr2) =
  unsafePerformIO $ do
    let !comp = comp1 <> comp2
        !totalLength = totalElem sz
    marr <- unsafeNew sz
    FArray _ _ resArr <- unsafeFreeze Seq marr
    withScheduler_ comp $ \scheduler -> do
      let schedule chunkStart chunkLength =
            let chunk1 = extractForeignArray chunkStart arr1
                chunk2 = extractForeignArray chunkStart arr2
                resChunk = extractForeignArray chunkStart resArr
             in scheduleWork_ scheduler $ f chunkLength chunk1 chunk2 resChunk
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


-- instance ReduceNumArray F Double where
--   evenPowerSumArrayS (FArray _ sz arr) =
--     unsafeInlineIO . SIMD.evenPowerSumForeignArray (totalSize sz) arr
--   {-# INLINE evenPowerSumArrayS #-}
--   absPowerSumArrayS (FArray _ sz arr) =
--     unsafeInlineIO . SIMD.absPowerSumForeignArray (totalSize sz) arr
--   {-# INLINE absPowerSumArrayS #-}
--   absMaxArrayS (FArray _ sz arr) = unsafeInlineIO $ SIMD.absMaxForeignArray (totalSize sz) arr
--   {-# INLINE absMaxArrayS #-}


instance Numeric F Double where
  sumArray (FArray _ sz arr) = unsafeInlineIO $ SIMD.sumForeignArray (totalSize sz) arr
  {-# INLINE sumArray #-}
  productArray (FArray _ sz arr) = unsafeInlineIO $ SIMD.productForeignArray (totalSize sz) arr
  {-# INLINE productArray #-}
  powerSumArray (FArray _ sz arr) =
    unsafeInlineIO . SIMD.evenPowerSumForeignArray (totalSize sz) arr
  {-# INLINE powerSumArray #-}
  unsafeDotProduct (FArray _ sz1 arr1) (FArray _ _ arr2) =
    unsafeInlineIO $ SIMD.multiplySumForeignArray (totalSize sz1) arr1 arr2
  {-# INLINE unsafeDotProduct #-}

  plusScalar arr x = splitApply (SIMD.plusScalarForeignArray x) arr
  {-# INLINE plusScalar #-}
  minusScalar arr x = splitApply (SIMD.minusScalarForeignArray x) arr
  {-# INLINE minusScalar #-}
  scalarMinus x arr = splitApply (SIMD.negatePlusScalarForeignArray x) arr
  {-# INLINE scalarMinus #-}

  multiplyScalar arr x = splitApply (SIMD.multiplyScalarForeignArray x) arr
  {-# INLINE multiplyScalar #-}
  absPointwise = splitApply SIMD.absForeignArray
  {-# INLINE absPointwise #-}
  additionPointwise = unsafeSplitApply2 SIMD.additionForeignArray
  {-# INLINE additionPointwise #-}
  subtractionPointwise = unsafeSplitApply2 SIMD.subtractionForeignArray
  {-# INLINE subtractionPointwise #-}
  multiplicationPointwise = unsafeSplitApply2 SIMD.multiplicationForeignArray
  {-# INLINE multiplicationPointwise #-}

  powerPointwise arr p = splitApply (SIMD.powerScalarForeignArray p) arr
  {-# INLINE powerPointwise #-}

  -- negatePlusScalar arr x = splitApply (SIMD.negatePlusScalarForeignArray x) arr
  -- {-# INLINE negatePlusScalar #-}
  foldArray f !initAcc arr = go initAcc 0
    where
      !len = totalElem (size arr)
      go !acc i
        | i < len = go (f acc (unsafeLinearIndex arr i)) (i + 1)
        | otherwise = acc
  {-# INLINE foldArray #-}
  unsafeLiftArray f arr = makeArrayLinear (getComp arr) (size arr) (f . unsafeLinearIndex arr)
  {-# INLINE unsafeLiftArray #-}
  unsafeLiftArray2 f a1 a2 =
    makeArrayLinear
      (getComp a1 <> getComp a2)
      (SafeSz (liftIndex2 min (unSz (size a1)) (unSz (size a2)))) $ \ !i ->
      f (unsafeLinearIndex a1 i) (unsafeLinearIndex a2 i)
  {-# INLINE unsafeLiftArray2 #-}

instance NumericFloat F Double where
  divideScalar arr x = splitApply (SIMD.divideScalarForeignArray x) arr
  {-# INLINE divideScalar #-}
  scalarDivide x arr = splitApply (SIMD.recipMultiplyForeignArray x) arr
  {-# INLINE scalarDivide #-}
  recipPointwise arr = splitApply (SIMD.recipPowerScalarForeignArray 1) arr
  {-# INLINE recipPointwise #-}
  divisionPointwise = unsafeSplitApply2 SIMD.divisionForeignArray
  {-# INLINE divisionPointwise #-}
  sqrtPointwise = splitApply SIMD.sqrtForeignArray
  {-# INLINE sqrtPointwise #-}

-- instance RoundFloatArray F Double Double where
--   roundPointwise = splitApply SIMD.roundForeignArray
--   {-# INLINE roundPointwise #-}

-- instance ReduceOrdArray F Double where
--   maximumArrayS e0 (FArray _ sz arr) =
--     unsafeInlineIO $ SIMD.maximumForeignArray e0 (totalSize sz) arr
--   {-# INLINE maximumArrayS #-}
--   minimumArrayS e0 (FArray _ sz arr) =
--     unsafeInlineIO $ SIMD.minimumForeignArray e0 (totalSize sz) arr
--   {-# INLINE minimumArrayS #-}


castFArray :: Array F ix a -> Array F ix e
castFArray (FArray c sz a) = FArray c sz (castForeignArray a)

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
