{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Massiv.Array.SIMD.DoubleSpec where

import Data.Massiv.Array as A
import Data.Massiv.Array.SIMD
import Data.Massiv.Core.Operations
import qualified Data.Vector.Storable as VS
import Test.Massiv.Core
import Test.Massiv.Core.Mutable

instance (VS.Storable a, Arbitrary a) => Arbitrary (VS.Vector a) where
    arbitrary = VS.fromList <$> arbitrary
    shrink = fmap VS.fromList . shrink . VS.toList


spec :: Spec
spec = do
  unsafeMutableSpec @F @Ix1 @Double
  unsafeMutableSpec @F @Ix2 @Double
  unsafeMutableSpec @F @Ix3 @Double
  unsafeMutableSpec @F @Ix4 @Double
  unsafeMutableSpec @F @Ix5 @Double
  unsafeMutableUnboxedSpec @F @Ix1 @Double
  unsafeMutableUnboxedSpec @F @Ix2 @Double
  unsafeMutableUnboxedSpec @F @Ix3 @Double
  unsafeMutableUnboxedSpec @F @Ix4 @Double
  unsafeMutableUnboxedSpec @F @Ix5 @Double
  let epsilon = 1e-11
  describe "Dot Product" $ do
    it "any" $
      property $ \(ArrSameSz x y :: ArrSameSz F Ix1 Double) ->
        epsilonEq epsilon (either throw id (dotProductM x y)) (A.sum (A.zipWith (*) x y))
    it "slice" $
      property $ \(ArrIx mat (i :. _)) ->
        let matP = computeAs P (mat :: Array F Ix2 Double)
            x = mat !> i
            y = matP !> i
         in epsilonEq epsilon (either throw id (dotProductM x x)) (A.sum (A.zipWith (*) y y))
    it "misaligned" $
      property $ \(ArrSameSz x' y' :: ArrSameSz F Ix1 Double) ->
        let sz = unSz (size x')
            x = extract' 0 (Sz (sz - 1)) x'
            y = extract' 1 (Sz (sz - 1)) y'
        in sz /= 0 ==>
           epsilonEq epsilon (either throw id (dotProductM x y)) (A.sum (A.zipWith (*) x y))
  describe "OuterSlice" $
    it "V vs P" $
    property $ \(ArrIx mat (i :. _)) ->
      let matP = computeAs P (mat :: Array F Ix2 Double)
          res1 = mat !> i
          res2 = matP !> i
       in delay res1 == delay res2
  describe "Matrix Multiplication" $ do
    it "transposed" $
      property $ \(ArrNE mat) -> monadicIO $ run $ do
        let matP = computeAs P mat
        res1 <- multiplyTransposed mat (mat :: Array F Ix2 Double)
        res2 <- multiplyTransposed matP matP
        pure $ epsilonArraysEq epsilon res1 res2
    it "|*|" $
      property $ \(ArrSameSz mat1 mat2) -> isNonEmpty (size mat1) ==> monadicIO $ run $ do
        let mat1P = computeAs P (mat1 :: Array F Ix2 Double)
            mat2P = computeAs P $ transpose mat2
        res1 <- mat1 |*| transpose mat2
        res2 <- mat1P |*| mat2P
        pure $ epsilonArraysEq epsilon res1 res2
  describe "Folding" $ do
    it "sum" $
      property $ \(arr :: Array D Ix1 Double) ->
        epsilonEq epsilon (A.sum arr) (sumArrayS (computeAs F arr))
    it "product" $
      property $ \(arr :: Array D Ix1 Double) ->
        epsilonEq epsilon (A.product arr) (productArrayS (computeAs F arr))
    it "evenPowerSum" $
      property $ \(arr :: Array D Ix1 Double) (Positive pow) ->
        even pow ==>
        epsilonEq epsilon (evenPowerSumArrayS arr pow) (evenPowerSumArrayS (computeAs F arr) pow)
    it "absPowerSum" $
      property $ \(arr :: Array D Ix1 Double) (Positive pow) ->
        epsilonEq epsilon (absPowerSumArrayS arr pow) (absPowerSumArrayS (computeAs F arr) pow)
    it "absPowerSum == powerSum even" $
      property $ \(arr :: Array F Ix1 Double) (Positive pow) ->
        even pow ==>
        epsilonEq epsilon (evenPowerSumArrayS arr pow) (absPowerSumArrayS arr pow)
    it "absMax" $
      property $ \(arr :: Array D Ix1 Double) ->
        epsilonEq epsilon (absMaxArrayS arr) (absMaxArrayS (computeAs F arr))
    it "normL1" $
      property $ \(arr :: Array F Ix1 Double) ->
        epsilonEq epsilon (normL1 (delay arr)) (normL1 arr)
    it "normL2" $
      property $ \(arr :: Array F Ix1 Double) ->
        epsilonEq epsilon (normL2 (delay arr)) (normL2 arr)
    it "normL3" $
      property $ \(arr :: Array F Ix1 Double) ->
        epsilonEq epsilon (normL3 (delay arr)) (normL3 arr)
    it "normL4" $
      property $ \(arr :: Array F Ix1 Double) ->
        epsilonEq epsilon (normL4 (delay arr)) (normL4 arr)
    it "normLpi" $
      property $ \(arr :: Array F Ix1 Double) -> monadicIO $ run $ do
        n1 <- normLpi 1 arr
        n2 <- normLpi 2 arr
        n3 <- normLpi 3 arr
        n4 <- normLpi 4 arr
        pure (counterexample "normL1" (epsilonEq epsilon (normL1 arr) n1) .&&.
              counterexample "normL2" (epsilonEq epsilon (normL2 arr) n2) .&&.
              counterexample "normL3" (epsilonEq epsilon (normL3 arr) n3) .&&.
              counterexample "normL4" (epsilonEq epsilon (normL4 arr) n4))
    it "maximum" $
      property $ \(ArrIx arr _ :: ArrIx D Ix1 Double) ->
        monadicIO $ run
        (epsilonEq epsilon <$> maximumM' arr <*> maximumM' (computeAs F arr))
    it "minimum" $
      property $ \(ArrIx arr _ :: ArrIx D Ix1 Double) ->
        monadicIO $ run
        (epsilonEq epsilon <$> minimumM' arr <*> minimumM' (computeAs F arr))
    -- it "eq" $
    --   property $ \(arr :: Array D Ix1 Double) ->
    --     eqDouble (computeAs F arr) (computeAs F arr)
    -- it "not eq" $
    --   property $ \(arr1 :: Array D Ix1 Double) (arr2 :: Array D Ix1 Double) ->
    --     arr1 /= arr2 ==> not (eqDouble (computeAs F arr1) (computeAs F arr2))
  describe "Mapping" $ do
    it "plus" $
      property $ \(arr :: Array D Ix1 Double) x ->
        epsilonArraysEq epsilon (arr .+ x) (computeAs F arr .+ x)
    it "minus" $
      property $ \(arr :: Array D Ix1 Double) x ->
        epsilonArraysEq epsilon (arr .- x) (computeAs F arr .- x)
    it "multiply" $
      property $ \(arr :: Array D Ix1 Double) x ->
        epsilonArraysEq epsilon (arr .* x) (computeAs F arr .* x)
    it "divide" $
      property $ \(arr :: Array D Ix1 Double) x ->
        epsilonArraysEq epsilon (arr ./ x) (computeAs F arr ./ x)
    it "multiplyDivide" $
      property $ \(arr :: Array D Ix1 Double) x ->
        epsilonArraysEq epsilon (x /. arr) (x /. computeAs F arr)
    it "abs" $
      property $ \(arr :: Array D Ix1 Double) ->
        arraysEq (abs arr) (abs (computeAs F arr))
    it "power" $
      property $ \(arr :: Array D Ix1 Double) (NonNegative pow) ->
        monadicIO $ run
          (epsilonArraysEq epsilon <$> (arr .^ pow) <*> (computeAs F arr .^ pow))
    it "sqrt" $
      property $ \(arr :: Array D Ix1 Double) ->
        epsilonArraysEq epsilon (sqrt arr) (sqrt (computeAs F arr))
    it "recip" $
      property $ \(arr :: Array D Ix1 Double) ->
        epsilonArraysEq epsilon (recip arr) (recip (computeAs F arr))
    it "round" $
      property $ \(arr :: Array D Ix1 Double) ->
        arraysEq (roundPointwise arr :: Array D Ix1 Double) (roundPointwise (computeAs F arr))
    -- it "truncate" $
    --   property $ \(arr :: Array D Ix1 Double) ->
    --     arraysEq (truncatePointwise arr :: Array D Ix1 Int64) (truncatePointwise (computeAs F arr))
  describe "Zipping Num" $ do
    it "addition" $
      property $ \(ArrSameSz arr1 arr2 :: ArrSameSz F Ix1 Double) ->
        epsilonArraysEq epsilon (delay arr1 + delay arr2) (arr1 + arr2)
    it "subtraction" $
      property $ \(ArrSameSz arr1 arr2 :: ArrSameSz F Ix1 Double) ->
        epsilonArraysEq epsilon (delay arr1 - delay arr2) (arr1 - arr2)
    it "multiplication" $
      property $ \(ArrSameSz arr1 arr2 :: ArrSameSz F Ix1 Double) ->
        epsilonArraysEq epsilon (delay arr1 * delay arr2) (arr1 * arr2)
    it "division" $
      property $ \(ArrSameSz arr1 arr2 :: ArrSameSz F Ix1 Double) ->
        epsilonArraysEq epsilon (delay arr1 / delay arr2) (arr1 / arr2)
  describe "Zipping" $ do
    it "addition" $
      property $ \(ArrSameSz arr1 arr2 :: ArrSameSz F Ix1 Double) ->
        monadicIO $ run $
        epsilonArraysEq epsilon <$> (delay arr1 .+. delay arr2) <*> (arr1 .+. arr2)
    it "subtraction" $
      property $ \(ArrSameSz arr1 arr2 :: ArrSameSz F Ix1 Double) ->
        monadicIO $ run $
        epsilonArraysEq epsilon <$> (delay arr1 .-. delay arr2) <*> (arr1 .-. arr2)
    it "multiplication" $
      property $ \(ArrSameSz arr1 arr2 :: ArrSameSz F Ix1 Double) ->
        monadicIO $ run $
        epsilonArraysEq epsilon <$> (delay arr1 .*. delay arr2) <*> (arr1 .*. arr2)
    it "division" $
      property $ \(ArrSameSz arr1 arr2 :: ArrSameSz F Ix1 Double) ->
        monadicIO $ run $
        epsilonArraysEq epsilon <$> (delay arr1 ./. delay arr2) <*> (arr1 ./. arr2)


data ArrSameSz r ix e = ArrSameSz !(Array r ix e) !(Array r ix e)

deriving instance Show (Array r ix e) => Show (ArrSameSz r ix e)

instance (Mutable r ix e, Arbitrary (Array D ix e)) => Arbitrary (ArrSameSz r ix e) where
  arbitrary = do
    a1 :: Array D ix e <- arbitrary
    a2 :: Array D ix e <- arbitrary
    let sz = liftSz2 min (size a1) (size a2)
    pure $ ArrSameSz (compute (extract' zeroIndex sz a1)) (compute (extract' zeroIndex sz a2))

arraysEq ::
     (Source r1 ix a, Source r2 ix a, Show a, Ord a)
  => Array r1 ix a
  -> Array r2 ix a
  -> Property
arraysEq arr1 arr2 =
  size arr1 === size arr2 .&&. foldlS (.&&.) (property True) (A.zipWith (===) arr1 arr2)


epsilonArraysEq ::
     (Source r1 ix a, Source r2 ix a, Show a, RealFloat a)
  => a
  -> Array r1 ix a
  -> Array r2 ix a
  -> Property
epsilonArraysEq epsilon arr1 arr2 =
  size arr1 === size arr2 .&&.
  foldlS (.&&.) (property True) (A.zipWith (epsilonEq epsilon) arr1 arr2)


epsilonEq ::
     (Show a, RealFloat a)
  => a -- ^ Epsilon, a maximum tolerated error. Sign is ignored.
  -> a -- ^ Expected result.
  -> a -- ^ Tested value.
  -> Property
epsilonEq epsilon x y =
  x === y .||. counterexample (show diff ++ " > " ++ show n) (diff <= n) .||. (isNaN x && isNaN y)
  where
    (absx, absy) = (abs x, abs y)
    n = epsilon * (1 + max absx absy)
    diff = abs (y - x)
