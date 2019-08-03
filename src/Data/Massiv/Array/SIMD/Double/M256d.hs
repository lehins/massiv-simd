-- |
-- Module      : Data.Massiv.Array.SIMD.Double.M256d
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.SIMD.Double.M256d where

import Data.Massiv.Array.ForeignArray
import Data.Massiv.Core.Index
import Foreign.C.Types
import Foreign.Ptr (Ptr)
import Data.Coerce


perAlignment :: Int
perAlignment = 4


multiplySumForeignArray ::
     Index ix => ForeignArray ix Double -> ForeignArray ix Double -> IO Double
multiplySumForeignArray =
  fold2WithAlignedForeignArray c_dot_product__m256d_a (\acc x y -> acc + x * y) 0 perAlignment
{-# INLINE multiplySumForeignArray #-}


eqForeignArray :: Index ix => ForeignArray ix Double -> ForeignArray ix Double -> IO Bool
eqForeignArray = eqWithForeignArray c_eq__m128d
{-# INLINE eqForeignArray #-}

additionForeignArray ::
     Index ix => ForeignArray ix Double -> ForeignArray ix Double -> ForeignArray ix Double -> IO ()
additionForeignArray = zipWithAlignedForeignArray c_addition__m256d_a (+) perAlignment
{-# INLINE additionForeignArray #-}


sumForeignArray :: Index ix => ForeignArray ix Double -> IO Double
sumForeignArray = foldWithAlignedForeignArray c_sum__m256d_a (+) 0 perAlignment
{-# INLINE sumForeignArray #-}

productForeignArray :: Index ix => ForeignArray ix Double -> IO Double
productForeignArray = coerce . foldWithForeignArray c_product__m128d
{-# INLINE productForeignArray #-}

maximumForeignArray :: Index ix => ForeignArray ix Double -> IO Double
maximumForeignArray = coerce . foldWithForeignArray c_maximum__m128d
{-# INLINE maximumForeignArray #-}

copyForeignArray :: ForeignArray ix1 Double -> Ix1 -> ForeignArray ix2 Double -> Ix1 -> Sz1 -> IO ()
copyForeignArray = copyWithForeignArray c_copy__m128d
{-# INLINE copyForeignArray #-}

fillForeignArray :: ForeignArray ix Double -> Ix1 -> Sz1 -> Double -> IO ()
fillForeignArray = fillWithForeignArray c_fill__m128d
{-# INLINE fillForeignArray #-}


foreign import ccall unsafe "m128d.c massiv_fill__m128d"
  c_fill__m128d :: Ptr CDouble -> CLong -> CDouble -> IO ()

foreign import ccall unsafe "m128d.c massiv_copy__m128d"
  c_copy__m128d :: Ptr CDouble -> Ptr CDouble -> CLong -> IO ()


foreign import ccall unsafe "m256d.c massiv_dot_product__m256d_a"
  c_dot_product__m256d_a :: CDouble -> Ptr CDouble -> Ptr CDouble -> CLong -> IO CDouble


foreign import ccall unsafe "m128d.c massiv_eq__m128d"
  c_eq__m128d :: Ptr CDouble -> Ptr CDouble -> CLong -> IO CBool

foreign import ccall unsafe "m256d.c massiv_addition__m256d_a"
  c_addition__m256d_a :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CLong -> IO ()

foreign import ccall unsafe "m128d.c massiv_addition__m128d"
  c_addition__m128d :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CLong -> IO ()


foreign import ccall safe "m256d.c massiv_sum__m256d_a"
  c_sum__m256d_a :: CDouble -> Ptr CDouble -> CLong -> IO CDouble


foreign import ccall safe "m128d.c massiv_product__m128d"
  c_product__m128d :: Ptr CDouble -> CLong -> IO CDouble


foreign import ccall safe "m128d.c massiv_maximum__m128d"
  c_maximum__m128d :: Ptr CDouble -> CLong -> IO CDouble

