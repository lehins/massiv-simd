{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Massiv.Array.SIMD.DoubleSpec where

import Data.Massiv.Array as A
import Data.Massiv.Array.SIMD.Double
import Data.Typeable
import qualified Data.Vector.Storable as VS
import Test.Hspec
import Test.QuickCheck as QC


-- | Arbitrary non-empty array with a valid index. Can be either `Seq` or `Par`
data ArrIx r ix e = ArrIx (Array r ix e) ix

deriving instance (Show (Array r ix e), Show ix) => Show (ArrIx r ix e)

-- | Non-empty size together with an index that is within bounds of that index.
data SzIx ix = SzIx (Sz ix) ix deriving Show

instance (Index ix, Arbitrary ix) => Arbitrary (SzIx ix) where
  arbitrary = do
    sz <- liftIndex (+1) . unSz . Sz <$> arbitrary
    -- Make sure index is within bounds:
    SzIx (Sz sz) . flip (liftIndex2 mod) sz <$> arbitrary

instance (Arbitrary ix, Typeable e, Construct r ix e, Arbitrary e) =>
         Arbitrary (ArrIx r ix e) where
  arbitrary = do
    SzIx sz ix <- arbitrary
    func <- arbitrary
    comp <- oneof [pure Seq, pure Par]
    return $ ArrIx (makeArrayLinear comp sz func) ix


-- | Arbitrary array
instance (Arbitrary ix, Typeable e, Construct r ix e, Arbitrary e) =>
         Arbitrary (Array r ix e) where
  arbitrary = do
    sz <- Sz <$> arbitrary
    func <- arbitrary
    comp <- oneof [pure Seq, pure Par]
    return $ makeArrayLinear comp sz func

instance Arbitrary Ix2 where
  arbitrary = (:.) <$> arbitraryIntIx <*> arbitraryIntIx

instance Arbitrary Ix3 where
  arbitrary = (:>) <$> arbitraryIntIx <*> ((:.) <$> arbitraryIntIx <*> arbitraryIntIx)

instance Arbitrary Ix4 where
  arbitrary = (:>) <$> arbitraryIntIx <*> arbitrary

instance Arbitrary Ix5 where
  arbitrary = (:>) <$> arbitraryIntIx <*> arbitrary

arbitraryIntIx :: Gen Int
arbitraryIntIx = sized (\s -> QC.resize (floor $ (sqrt :: Double -> Double) $ fromIntegral s) arbitrary)


instance (VS.Storable a, Arbitrary a) => Arbitrary (VS.Vector a) where
    arbitrary = VS.fromList <$> arbitrary
    shrink = fmap VS.fromList . shrink . VS.toList


spec :: Spec
spec = do
  let epsilon = 0.000000000001
  describe "Dot Product" $ do
    it "even" $
      property $ \x y ->
        even (unSz (size x)) &&
        even (unSz (size y)) ==>
        epsilonEq epsilon (dotDouble x y) (A.sum (A.zipWith (*) x y))
    it "any" $
      property $ \x y ->
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


epsilonEq :: (Num a, Ord a) =>
             a -- ^ Epsilon, a maximum tolerated error. Sign is ignored.
          -> a -- ^ Expected result.
          -> a -- ^ Tested value.
          -> Bool
epsilonEq epsilon x y = x == y || abs (y - x) <= n * epsilon
  where (absx, absy) = (abs x, abs y)
        n = 1 + if absx < absy then absy else absx

a :: Array V Ix2 Double
a = fromLists' Seq [[1.452424542985271], [-0.8913366027292209]]
