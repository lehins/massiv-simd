{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Massiv.Array.SIMD.Double.M128d
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.SIMD.Double.M128d where

import Data.Massiv.Array.ForeignArray
import Data.Massiv.Core.Index
import Foreign.C.Types
import Foreign.Ptr (Ptr)
import Data.Coerce


perAlignment :: Int
perAlignment = 2

dotProductForeignArray ::
     Index ix => Sz1 -> ForeignArray ix Double -> ForeignArray ix Double -> IO Double
dotProductForeignArray =
  fold2WithAlignedForeignArray c_dot_product__m128d_a (\acc x y -> acc + x * y) 0 perAlignment
{-# INLINE dotProductForeignArray #-}


eqForeignArray :: Index ix => Sz1 -> ForeignArray ix Double -> ForeignArray ix Double -> IO Bool
eqForeignArray = eqWithForeignArray c_eq__m128d
{-# INLINE eqForeignArray #-}


additionForeignArray ::
     Index ix => ForeignArray ix Double -> ForeignArray ix Double -> ForeignArray ix Double -> IO ()
additionForeignArray = zipWithForeignArray c_addition__m128d
{-# INLINE additionForeignArray #-}

sumForeignArray :: Index ix => ForeignArray ix Double -> IO Double
sumForeignArray = foldWithAlignedForeignArray c_sum__m128d_a (+) 0 perAlignment
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

setForeignArray :: ForeignArray ix Double -> Ix1 -> Sz1 -> Double -> IO ()
setForeignArray = setWithForeignArray c_set__m128d
{-# INLINE setForeignArray #-}


foreign import ccall unsafe "m128d.c massiv_set__m128d"
  c_set__m128d :: Ptr CDouble -> CLong -> CDouble -> IO ()

foreign import ccall unsafe "m128d.c massiv_copy__m128d"
  c_copy__m128d :: Ptr CDouble -> Ptr CDouble -> CLong -> IO ()


foreign import ccall unsafe "m128d.c massiv_dot_product__m128d_a"
  c_dot_product__m128d_a :: CDouble -> Ptr CDouble -> Ptr CDouble -> CLong -> IO CDouble

foreign import ccall unsafe "m128d.c massiv_eq__m128d"
  c_eq__m128d :: Ptr CDouble -> Ptr CDouble -> CLong -> IO CBool


foreign import ccall unsafe "m128d.c massiv_addition__m128d"
  c_addition__m128d :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CLong -> IO ()


foreign import ccall safe "m128d.c massiv_sum__m128d_a"
  c_sum__m128d_a :: CDouble -> Ptr CDouble -> CLong -> IO CDouble


foreign import ccall safe "m128d.c massiv_product__m128d"
  c_product__m128d :: Ptr CDouble -> CLong -> IO CDouble


foreign import ccall safe "m128d.c massiv_maximum__m128d"
  c_maximum__m128d :: Ptr CDouble -> CLong -> IO CDouble

