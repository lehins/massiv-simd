{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Massiv.Array.SIMD.DoubleSpec where

import Data.Massiv.Array as A
import Data.Massiv.Array.SIMD.Double
import Data.Typeable
import qualified Data.Vector.Storable as VS
import Test.Hspec
import Test.QuickCheck as QC


-- | Arbitrary array
instance (CoArbitrary ix, Arbitrary ix, Typeable e, Construct r ix e, Arbitrary e) =>
         Arbitrary (Array r ix e) where
  arbitrary = do
    sz <- Sz <$> arbitrary
    func <- arbitrary
    comp <- oneof [pure Seq, pure Par]
    return $ makeArray comp sz func

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
  describe "Dot Product" $
    -- it "Storable " $ property $ \ x y -> epsilonEq 0.000000000001 (dotDoubleSumS x y) (A.sum (A.zipWith (*) x y))
    -- it "Vector Storable " $ property $ \ x y -> epsilonEq 0.000000000001 (dotVectorS x y) (VS.sum (VS.zipWith (*) x y))
    -- it "storable" $ property $ \ x y -> epsilonEq 0.000000000001 (dotDoubleS x y) (A.sum (A.zipWith (*) x y))
   do
    it "even" $
      property $ \x y ->
        even (unSz (size x)) &&
        even (unSz (size y)) ==>
        epsilonEq 0.000000000001 (dotDouble x y) (A.sum (A.zipWith (*) x y))
    it "simple" $
      property $ \x y ->
        epsilonEq 0.000000000001 (dotDouble x y) (A.sum (A.zipWith (*) x y))
    -- it "simple" $ property $ \ x y -> (A.sum (A.zipWith (*) x y) === A.sum (A.zipWith (*) (x :: Array V Ix1 Double) (y :: Array V Ix1 Double)))
    --it "simple" $ property $ \ x y -> dotDouble x y === A.sum (A.zipWith (*) x y)


epsilonEq :: (Num a, Ord a, Show a) =>
             a -- ^ Epsilon, a maximum tolerated error. Sign is ignored.
          -> a -- ^ Expected result.
          -> a -- ^ Tested value.
          -> Property
epsilonEq epsilon x y = x === y .||. abs (y - x) <= n * epsilon
  where (absx, absy) = (abs x, abs y)
        n = 1 + if absx < absy then absy else absx
