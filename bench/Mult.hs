{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Massiv.Array.SIMD as A
import Data.Massiv.Bench.Matrix

main :: IO ()
main = do
  defaultMain
    [ benchMxM (randomMxM :: MxM P Double)
    , benchMxM (randomMxM :: MxM F Double)
    , benchVxM (randomVxM :: VxM P Double)
    , benchVxM (randomVxM :: VxM F Double)
    , benchMxV (randomMxV :: MxV P Double)
    , benchMxV (randomMxV :: MxV F Double)
    ]
