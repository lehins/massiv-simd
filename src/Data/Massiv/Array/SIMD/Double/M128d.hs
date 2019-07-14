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


doubleAlignment :: Int
doubleAlignment = 16

dotDouble :: Index ix => ForeignArray ix Double -> ForeignArray ix Double -> IO Double
dotDouble = coerce . fold2WithForeignArray c_dot__m128d
{-# INLINE dotDouble #-}

eqDouble :: Index ix => ForeignArray ix Double -> ForeignArray ix Double -> IO Bool
eqDouble = eqWithForeignArray c_eq__m128d
{-# INLINE eqDouble #-}


plusDouble ::
     Index ix
  => ForeignArray ix Double
  -> ForeignArray ix Double
  -> IO (ForeignArray ix Double)
plusDouble = zipWithForeignArray c_plus__m128d
{-# INLINE plusDouble #-}


sumDouble :: Index ix => ForeignArray ix Double -> IO Double
sumDouble = coerce . foldWithForeignArray c_sum__m128d
{-# INLINE sumDouble #-}

productDouble :: Index ix => ForeignArray ix Double -> IO Double
productDouble = coerce . foldWithForeignArray c_product__m128d
{-# INLINE productDouble #-}

maximumDouble :: Index ix => ForeignArray ix Double -> IO Double
maximumDouble = coerce . foldWithForeignArray c_maximum__m128d
{-# INLINE maximumDouble #-}

copyDouble :: ForeignArray ix1 Double -> Ix1 -> ForeignArray ix2 Double -> Ix1 -> Sz1 -> IO ()
copyDouble = copyWithForeignArray c_copy__m128d
{-# INLINE copyDouble #-}

broadcastDouble :: ForeignArray ix Double -> Ix1 -> Sz1 -> Double -> IO ()
broadcastDouble = setWithForeignArray c_broadcast__m128d
{-# INLINE broadcastDouble #-}


foreign import ccall unsafe "m128d.c massiv_broadcast__m128d"
  c_broadcast__m128d :: Ptr CDouble -> CLong -> CDouble -> IO ()

foreign import ccall unsafe "m128d.c massiv_copy__m128d"
  c_copy__m128d :: Ptr CDouble -> Ptr CDouble -> CLong -> IO ()


foreign import ccall unsafe "m128d.c massiv_dot__m128d"
  c_dot__m128d :: Ptr CDouble -> Ptr CDouble -> CLong -> IO CDouble

foreign import ccall unsafe "m128d.c massiv_eq__m128d"
  c_eq__m128d :: Ptr CDouble -> Ptr CDouble -> CLong -> IO CBool


foreign import ccall unsafe "m128d.c massiv_plus__m128d"
  c_plus__m128d :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CLong -> IO ()


foreign import ccall safe "m128d.c massiv_sum__m128d"
  c_sum__m128d :: Ptr CDouble -> CLong -> IO CDouble


foreign import ccall safe "m128d.c massiv_product__m128d"
  c_product__m128d :: Ptr CDouble -> CLong -> IO CDouble


foreign import ccall safe "m128d.c massiv_maximum__m128d"
  c_maximum__m128d :: Ptr CDouble -> CLong -> IO CDouble

