{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Massiv.Array.SIMD as A
import Data.Massiv.Bench.Vector


main :: IO ()
main = do
  defaultMain
    [ benchV1 (randomV1 :: Vector P Double)
    , benchV1 (randomV1 :: Vector F Double)
    , benchVxV (randomVxV :: VxV P Double)
    , benchVxV (randomVxV :: VxV F Double)
    ]
