{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Massiv.Array.SIMD.DoubleSpec where

import Data.Massiv.Array as A
import Test.Massiv.Core
import Test.Massiv.Core.Mutable
import Data.Massiv.Array.SIMD
import qualified Data.Vector.Storable as VS

instance (VS.Storable a, Arbitrary a) => Arbitrary (VS.Vector a) where
    arbitrary = VS.fromList <$> arbitrary
    shrink = fmap VS.fromList . shrink . VS.toList


spec :: Spec
spec = do
  unsafeMutableSpec @V @Ix1 @Double
  unsafeMutableSpec @V @Ix2 @Double
  unsafeMutableSpec @V @Ix3 @Double
  unsafeMutableSpec @V @Ix4 @Double
  unsafeMutableSpec @V @Ix5 @Double
  unsafeMutableUnboxedSpec @V @Ix1 @Double
  unsafeMutableUnboxedSpec @V @Ix2 @Double
  unsafeMutableUnboxedSpec @V @Ix3 @Double
  unsafeMutableUnboxedSpec @V @Ix4 @Double
  unsafeMutableUnboxedSpec @V @Ix5 @Double
  let epsilon = 0.000000000001
  describe "Dot Product" $ do
    it "any" $
      property $ \x (y :: Array V Ix1 Double) ->
        epsilonEq epsilon (dotDouble x y) (A.sum (A.zipWith (*) x y))
    it "slice" $
      property $ \(ArrIx mat (i :. _)) ->
        let matP = computeAs P (mat :: Array V Ix2 Double)
            x = mat !> i
            y = matP !> i
         in epsilonEq epsilon (dotDouble x x) (A.sum (A.zipWith (*) y y))
  describe "OuterSlice" $
    it "V vs P" $
    property $ \(ArrIx mat (i :. _)) ->
      let matP = computeAs P (mat :: Array V Ix2 Double)
          res1 = mat !> i
          res2 = matP !> i
       in delay res1 == delay res2
  describe "Matrix Multiplication" $ do
    it "V vs P" $
      property $ \(ArrIx mat _) ->
        let matP = computeAs P mat
            res1 = multiplyTransposed mat (mat :: Array V Ix2 Double)
            res2 = multiplyTransposed matP matP
         in A.and $ A.zipWith (epsilonEq epsilon) res1 res2
    it "transposed" $
      property $ \(ArrIx mat _) ->
        let res1 = multiplyTransposed mat (mat :: Array V Ix2 Double)
            res2 = multiplyTransposedSIMD mat $ computeAs V mat
         in A.and $ A.zipWith (epsilonEq epsilon) res1 res2
  describe "Folding" $ do
    it "sum" $
      property $ \(arr :: Array D Ix1 Double) ->
        epsilonEq epsilon (A.sum arr) (sumDouble (computeAs V arr))
    it "product" $
      property $ \(arr :: Array D Ix1 Double) ->
        epsilonEq epsilon (A.product arr) (productDouble (computeAs V arr))
    it "maximum" $
      property $ \(ArrIx arr _ :: ArrIx D Ix1 Double) ->
        epsilonEq epsilon (A.maximum' arr) (maximumDouble (computeAs V arr))
    it "eq" $
      property $ \(arr :: Array D Ix1 Double) ->
        eqDouble (computeAs V arr) (computeAs V arr)
    it "not eq" $
      property $ \(arr1 :: Array D Ix1 Double) (arr2 :: Array D Ix1 Double) ->
        arr1 /= arr2 ==> not (eqDouble (computeAs V arr1) (computeAs V arr2))


epsilonEq :: (Num a, Ord a) =>
             a -- ^ Epsilon, a maximum tolerated error. Sign is ignored.
          -> a -- ^ Expected result.
          -> a -- ^ Tested value.
          -> Bool
epsilonEq epsilon x y = x == y || abs (y - x) <= n * epsilon
  where (absx, absy) = (abs x, abs y)
        n = 1 + if absx < absy then absy else absx

