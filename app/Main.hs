{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where


import Test.Hspec
import Test.QuickCheck as QC
import Data.Massiv.Array as A
import Data.Massiv.Array.SIMD.Double
import Data.Typeable
import Control.Concurrent



-- | Arbitrary array
instance (CoArbitrary ix, Arbitrary ix, Typeable e, Construct r ix e, Arbitrary e) =>
         Arbitrary (Array r ix e) where
  arbitrary = do
    sz <- Sz <$> arbitrary
    func <- arbitrary
    comp <- oneof [pure Seq, pure Par]
    return $ makeArray comp sz func

arbitraryIntIx :: Gen Int
arbitraryIntIx = sized (\s -> QC.resize (floor $ (sqrt :: Double -> Double) $ fromIntegral s) arbitrary)

x :: Array S Ix1 Double
x =
  [ -33.97215764651025, 6.071086670960917, -5.213994635034543, -45.3823813342025, 28.59113447146227, -17.796767034448674, 25.37241288981908, -38.725435076529166, -45.40310304724634, -10.46281956503895, -21.95206398724936, 42.40957011907271, 9.362026018907606, -31.836723164433437, -21.52274895125065, 38.66119844385658, -27.104875921853914, -28.637775413838853, -35.15811903325399, -26.939448523573695, -45.28312629427476, 39.10682748733077, -45.69897826883162, -42.30332088304959, 38.43928552366011, 8.211583096497122, 2.1083444388285004, 38.921472186028254, 3.323061317444519 ]

y :: Array S Ix1 Double
y =
  [ 19.20779609650955, -35.17472908426549, -46.279534687596865, -35.19335043740887, 7.700118708336558, 9.356644829438308, 2.2435579833703985, 1.3762805917604428, 3.844869584079779, 48.84941458293603, -15.741805406868606, -44.91454093630103, -12.451713257287453, 46.10800768678423, -2.790502122562691, 48.519636811082435, -3.3786771671976648, 29.04239457114812, -33.21294387829512, -5.043476451207771, 25.614541749543594 ]




main :: IO ()
main = do
  -- a <- generate $ QC.resize 50 arbitrary
  -- b <- generate $ QC.resize 50 arbitrary
  -- print a
  -- print b
  -- threadDelay 1000000
  -- print $ dotDoubleS a b
  print $ dotDoubleS x y


