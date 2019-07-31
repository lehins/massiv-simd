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


multiplySumForeignArray ::
     Index ix => ForeignArray ix Double -> ForeignArray ix Double -> IO Double
multiplySumForeignArray =
  fold2WithAlignedForeignArray c_dot_product__m128d_a (\acc x y -> acc + x * y) 0 perAlignment
{-# INLINE multiplySumForeignArray #-}


eqForeignArray :: Index ix => ForeignArray ix Double -> ForeignArray ix Double -> IO Bool
eqForeignArray = eqWithAlignedForeignArray c_eq__m128d_a perAlignment
{-# INLINE eqForeignArray #-}


-- eqForeignArray :: Index ix => ForeignArray ix Double -> ForeignArray ix Double -> IO Bool
-- eqForeignArray = eqWithForeignArray c_eq__m128d
-- {-# INLINE eqForeignArray #-}

plusScalarForeignArray ::
     Index ix
  => ForeignArray ix Double
  -> Double
  -> ForeignArray ix Double
  -> IO ()
plusScalarForeignArray arr x =
  liftAlignedForeignArray (`c_plus__m128d_a` coerce x) (+ x) perAlignment arr
{-# INLINE plusScalarForeignArray #-}

minusScalarForeignArray ::
     Index ix
  => ForeignArray ix Double
  -> Double
  -> ForeignArray ix Double
  -> IO ()
minusScalarForeignArray arr x =
  liftAlignedForeignArray (`c_minus__m128d_a` coerce x) (subtract x) perAlignment arr
{-# INLINE minusScalarForeignArray #-}


multiplyScalarForeignArray ::
     Index ix
  => ForeignArray ix Double
  -> Double
  -> ForeignArray ix Double
  -> IO ()
multiplyScalarForeignArray arr x =
  liftAlignedForeignArray (`c_multiply__m128d_a` coerce x) (* x) perAlignment arr
{-# INLINE multiplyScalarForeignArray #-}

absPointwiseForeignArray ::
     Index ix
  => ForeignArray ix Double
  -> ForeignArray ix Double
  -> IO ()
absPointwiseForeignArray = liftAlignedForeignArray c_abs__m128d_a abs perAlignment
{-# INLINE absPointwiseForeignArray #-}

additionForeignArray ::
     Index ix
  => ForeignArray ix Double
  -> ForeignArray ix Double
  -> ForeignArray ix Double
  -> IO ()
additionForeignArray = zipWithAlignedForeignArray c_addition__m128d_a (+) perAlignment
{-# INLINE additionForeignArray #-}

subtractionForeignArray ::
     Index ix
  => ForeignArray ix Double
  -> ForeignArray ix Double
  -> ForeignArray ix Double
  -> IO ()
subtractionForeignArray = zipWithAlignedForeignArray c_subtraction__m128d_a (-) perAlignment
{-# INLINE subtractionForeignArray #-}


multiplicationForeignArray ::
     Index ix
  => ForeignArray ix Double
  -> ForeignArray ix Double
  -> ForeignArray ix Double
  -> IO ()
multiplicationForeignArray = zipWithAlignedForeignArray c_multiplication__m128d_a (*) perAlignment
{-# INLINE multiplicationForeignArray #-}


sumForeignArray :: Index ix => ForeignArray ix Double -> IO Double
sumForeignArray = foldWithAlignedForeignArray c_sum__m128d_a (+) 0 perAlignment
{-# INLINE sumForeignArray #-}

productForeignArray :: Index ix => ForeignArray ix Double -> IO Double
productForeignArray = foldWithAlignedForeignArray c_product__m128d_a (*) 1 perAlignment
{-# INLINE productForeignArray #-}

powerSumForeignArray :: Index ix => ForeignArray ix Double -> Int -> IO Double
powerSumForeignArray arr pow =
  foldWithAlignedForeignArray
    (c_power_sum__m128d_a (fromIntegral pow))
    (powerSum pow)
    0
    perAlignment
    arr
{-# INLINE powerSumForeignArray #-}

powerSum :: (Integral b, Num a) => b -> a -> a -> a
powerSum pow acc x = acc + x ^ pow
{-# INLINE powerSum #-}

absPowerSumForeignArray :: Index ix => ForeignArray ix Double -> Int -> IO Double
absPowerSumForeignArray arr pow =
  foldWithAlignedForeignArray
    (c_abs_power_sum__m128d_a (fromIntegral pow))
    (\acc -> powerSum pow acc . abs)
    0
    perAlignment
    arr
{-# INLINE absPowerSumForeignArray #-}


maximumForeignArray :: Index ix => ForeignArray ix Double -> IO Double
maximumForeignArray = coerce . foldWithForeignArray c_maximum__m128d
{-# INLINE maximumForeignArray #-}

copyForeignArray :: Index ix => ForeignArray ix Double -> ForeignArray ix Double -> IO ()
copyForeignArray = copyWithAlignedForeignArray c_copy__m128d_a perAlignment
{-# INLINE copyForeignArray #-}

fillForeignArray :: Index ix => Double -> ForeignArray ix Double -> IO ()
fillForeignArray = fillWithAlignedForeignArray c_fill__m128d_a perAlignment
{-# INLINE fillForeignArray #-}



foreign import ccall unsafe "m128d.c massiv_fill__m128d_a"
  c_fill__m128d_a :: CDouble -> Ptr CDouble -> CLong -> IO ()

foreign import ccall unsafe "m128d.c massiv_copy__m128d_a"
  c_copy__m128d_a :: Ptr CDouble -> Ptr CDouble -> CLong -> IO ()


foreign import ccall unsafe "m128d.c massiv_dot_product__m128d_a"
  c_dot_product__m128d_a :: CDouble -> Ptr CDouble -> Ptr CDouble -> CLong -> IO CDouble

foreign import ccall unsafe "m128d.c massiv_eq__m128d_a"
  c_eq__m128d_a :: Ptr CDouble -> Ptr CDouble -> CLong -> IO CBool


foreign import ccall unsafe "m128d.c massiv_plus__m128d_a"
  c_plus__m128d_a :: Ptr CDouble -> CDouble -> Ptr CDouble -> CLong -> IO ()

foreign import ccall unsafe "m128d.c massiv_minus__m128d_a"
  c_minus__m128d_a :: Ptr CDouble -> CDouble -> Ptr CDouble -> CLong -> IO ()

foreign import ccall unsafe "m128d.c massiv_multiply__m128d_a"
  c_multiply__m128d_a :: Ptr CDouble -> CDouble -> Ptr CDouble -> CLong -> IO ()

foreign import ccall unsafe "m128d.c massiv_abs__m128d_a"
  c_abs__m128d_a :: Ptr CDouble -> Ptr CDouble -> CLong -> IO ()

foreign import ccall unsafe "m128d.c massiv_addition__m128d_a"
  c_addition__m128d_a :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CLong -> IO ()

foreign import ccall unsafe "m128d.c massiv_subtraction__m128d_a"
  c_subtraction__m128d_a :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CLong -> IO ()

foreign import ccall unsafe "m128d.c massiv_multiplication__m128d_a"
  c_multiplication__m128d_a :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CLong -> IO ()


foreign import ccall safe "m128d.c massiv_sum__m128d_a"
  c_sum__m128d_a :: CDouble -> Ptr CDouble -> CLong -> IO CDouble

foreign import ccall safe "m128d.c massiv_product__m128d_a"
  c_product__m128d_a :: CDouble -> Ptr CDouble -> CLong -> IO CDouble

foreign import ccall safe "m128d.c massiv_power_sum__m128d_a"
  c_power_sum__m128d_a :: CLong -> CDouble -> Ptr CDouble -> CLong -> IO CDouble

foreign import ccall safe "m128d.c massiv_abs_power_sum__m128d_a"
  c_abs_power_sum__m128d_a :: CLong -> CDouble -> Ptr CDouble -> CLong -> IO CDouble


foreign import ccall safe "m128d.c massiv_maximum__m128d"
  c_maximum__m128d :: Ptr CDouble -> CLong -> IO CDouble


foreign import ccall unsafe "m128d.c massiv_addition__m128d"
  c_addition__m128d :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CLong -> IO ()
