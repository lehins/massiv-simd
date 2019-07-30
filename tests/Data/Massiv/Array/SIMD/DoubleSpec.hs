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
      property $ \x (y :: Array F Ix1 Double) ->
        epsilonEq epsilon (dotProduct x y) (A.sum (A.zipWith (*) x y))
    it "slice" $
      property $ \(ArrIx mat (i :. _)) ->
        let matP = computeAs P (mat :: Array F Ix2 Double)
            x = mat !> i
            y = matP !> i
         in epsilonEq epsilon (dotProduct x x) (A.sum (A.zipWith (*) y y))
    it "misaligned" $
      property $ \(ArrNE x :: ArrNE F Ix1 Double) (y :: Array F Ix1 Double) ->
        let x' = extract' 1 (Sz (unSz (size x) - 1)) x
        in epsilonEq epsilon (dotProduct x' y) (A.sum (A.zipWith (*) x' y))
  describe "OuterSlice" $
    it "V vs P" $
    property $ \(ArrIx mat (i :. _)) ->
      let matP = computeAs P (mat :: Array F Ix2 Double)
          res1 = mat !> i
          res2 = matP !> i
       in delay res1 == delay res2
  describe "Matrix Multiplication" $
    it "transposed" $
      property $ \(ArrNE mat) -> do
        let matP = computeAs P mat
        res1 <- multiplyTransposed mat (mat :: Array F Ix2 Double)
        res2 <- multiplyTransposed matP matP
        A.and (A.zipWith (epsilonEq epsilon) res1 res2) `shouldBe` True
  describe "Folding" $ do
    it "sum" $
      property $ \(arr :: Array D Ix1 Double) ->
        epsilonEq epsilon (A.sum arr) (sumArrayS (computeAs F arr))
    it "product" $
      property $ \(arr :: Array D Ix1 Double) ->
        epsilonEq epsilon (A.product arr) (productArrayS (computeAs F arr))
    it "powerSum" $
      property $ \(arr :: Array D Ix1 Double) (NonNegative pow) ->
        epsilonEq epsilon (powerSumArrayS arr pow) (powerSumArrayS (computeAs F arr) pow)
    it "absPowerSum == powerSum even" $
      property $ \(arr :: Array F Ix1 Double) (Positive pow) ->
        even pow ==>
        epsilonEq epsilon (powerSumArrayS arr pow) (absPowerSumArrayS arr pow)
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
    it "normLn" $
      property $ \(arr :: Array F Ix1 Double) -> do
        n1 <- normLn 1 arr
        n2 <- normLn 2 arr
        n3 <- normLn 3 arr
        n4 <- normLn 4 arr
        epsilonEq epsilon (normL1 arr) n1 `shouldBe` True
        epsilonEq epsilon (normL2 arr) n2 `shouldBe` True
        epsilonEq epsilon (normL3 arr) n3 `shouldBe` True
        epsilonEq epsilon (normL4 arr) n4 `shouldBe` True
    -- it "maximum" $
    --   property $ \(ArrIx arr _ :: ArrIx D Ix1 Double) ->
    --     epsilonEq epsilon (A.maximum' arr) (maximumDouble (Proxy :: Proxy V) (computeAs F arr))
    -- it "eq" $
    --   property $ \(arr :: Array D Ix1 Double) ->
    --     eqDouble (computeAs F arr) (computeAs F arr)
    -- it "not eq" $
    --   property $ \(arr1 :: Array D Ix1 Double) (arr2 :: Array D Ix1 Double) ->
    --     arr1 /= arr2 ==> not (eqDouble (computeAs F arr1) (computeAs F arr2))
  describe "Mapping" $ do
    it "Plus" $
      property $ \(arr :: Array D Ix1 Double) x ->
        A.and $ A.zipWith (epsilonEq epsilon) (arr .+ x) (computeAs F arr .+ x)
    it "Minus" $
      property $ \(arr :: Array D Ix1 Double) x ->
        A.and $ A.zipWith (epsilonEq epsilon) (arr .- x) (computeAs F arr .- x)
    it "Multiply" $
      property $ \(arr :: Array D Ix1 Double) x ->
        A.and $ A.zipWith (epsilonEq epsilon) (arr .* x) (computeAs F arr .* x)
    it "Abs" $
      property $ \(arr :: Array D Ix1 Double) ->
        A.and $ A.zipWith (==) (abs arr) (abs (computeAs F arr))
    it "Power" $
      property $ \(arr :: Array D Ix1 Double) (NonNegative pow) ->
        A.and $ A.zipWith (epsilonEq epsilon) (arr .^ pow) (computeAs F arr .^ pow)
  describe "Zipping" $ do
    it "addition" $
      property $ \(ArrSameSz arr1 arr2 :: ArrSameSz F Ix1 Double) ->
        A.and $ A.zipWith (epsilonEq epsilon) (delay arr1 + delay arr2) (arr1 + arr2)
    it "subtraction" $
      property $ \(ArrSameSz arr1 arr2 :: ArrSameSz F Ix1 Double) ->
        A.and $ A.zipWith (epsilonEq epsilon) (delay arr1 - delay arr2) (arr1 - arr2)
    it "multiplication" $
      property $ \(ArrSameSz arr1 arr2 :: ArrSameSz F Ix1 Double) ->
        A.and $ A.zipWith (epsilonEq epsilon) (delay arr1 * delay arr2) (arr1 * arr2)


data ArrSameSz r ix e = ArrSameSz !(Array r ix e) !(Array r ix e)

deriving instance Show (Array r ix e) => Show (ArrSameSz r ix e)

instance (Mutable r ix e, Arbitrary (Array D ix e)) => Arbitrary (ArrSameSz r ix e) where
  arbitrary = do
    a1 :: Array D ix e <- arbitrary
    a2 :: Array D ix e <- arbitrary
    let sz = liftSz2 min (size a1) (size a2)
    pure $ ArrSameSz (compute (extract' zeroIndex sz a1)) (compute (extract' zeroIndex sz a2))



epsilonEq :: (Num a, Ord a) =>
             a -- ^ Epsilon, a maximum tolerated error. Sign is ignored.
          -> a -- ^ Expected result.
          -> a -- ^ Tested value.
          -> Bool
epsilonEq epsilon x y = x == y || abs (y - x) <= n * epsilon
  where (absx, absy) = (abs x, abs y)
        n = 1 + if absx < absy then absy else absx

