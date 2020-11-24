-- |
-- Module      : Data.Massiv.Array.SIMD.Double.M128d
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.SIMD.Double.M128d where

import Data.Coerce
import Data.Int
import Data.Massiv.Array.ForeignArray
import Data.Massiv.Core.Index
--import Data.Massiv.Core.Operations (roundDouble)
import Foreign.C.Types
import Foreign.Ptr (Ptr)
import GHC.Float (double2Int)

perAlignment :: Int
perAlignment = 2


multiplySumForeignArray :: Sz1 -> ForeignArray Double -> ForeignArray Double -> IO Double
multiplySumForeignArray = multiplySumAlignedForeignArray c_dot_product__m128d_a perAlignment
{-# INLINE multiplySumForeignArray #-}


eqForeignArray :: Sz1 -> ForeignArray Double -> ForeignArray Double -> IO Bool
eqForeignArray = eqWithAlignedForeignArray c_eq__m128d_a perAlignment
{-# INLINE eqForeignArray #-}

plusScalarForeignArray :: Double -> Sz1 -> ForeignArray Double -> ForeignArray Double -> IO ()
plusScalarForeignArray x = liftAlignedForeignArray (`c_plus__m128d_a` coerce x) (+ x) perAlignment
{-# INLINE plusScalarForeignArray #-}

minusScalarForeignArray :: Double -> Sz1 -> ForeignArray Double -> ForeignArray Double -> IO ()
minusScalarForeignArray x =
  liftAlignedForeignArray (`c_minus__m128d_a` coerce x) (subtract x) perAlignment
{-# INLINE minusScalarForeignArray #-}

negatePlusScalarForeignArray ::
     Double -> Sz1 -> ForeignArray Double -> ForeignArray Double -> IO ()
negatePlusScalarForeignArray x =
  liftAlignedForeignArray (`c_negate_plus__m128d_a` coerce x) (x -) perAlignment
{-# INLINE negatePlusScalarForeignArray #-}


multiplyScalarForeignArray :: Double -> Sz1 -> ForeignArray Double -> ForeignArray Double -> IO ()
multiplyScalarForeignArray x =
  liftAlignedForeignArray (`c_multiply__m128d_a` coerce x) (* x) perAlignment
{-# INLINE multiplyScalarForeignArray #-}

powerScalarForeignArray :: Int -> Sz1 -> ForeignArray Double -> ForeignArray Double -> IO ()
powerScalarForeignArray x =
  liftAlignedForeignArray (`c_power__m128d_a` fromIntegral x) (^ x) perAlignment
{-# INLINE powerScalarForeignArray #-}

recipPowerScalarForeignArray :: Int -> Sz1 -> ForeignArray Double -> ForeignArray Double -> IO ()
recipPowerScalarForeignArray x =
  liftAlignedForeignArray (`c_recip_power__m128d_a` fromIntegral x) ((^ x) . recip) perAlignment
{-# INLINE recipPowerScalarForeignArray #-}

divideScalarForeignArray :: Double -> Sz1 -> ForeignArray Double -> ForeignArray Double -> IO ()
divideScalarForeignArray x =
  liftAlignedForeignArray (`c_divide__m128d_a` coerce x) (/ x) perAlignment
{-# INLINE divideScalarForeignArray #-}

recipMultiplyForeignArray :: Double -> Sz1 -> ForeignArray Double -> ForeignArray Double -> IO ()
recipMultiplyForeignArray x =
  liftAlignedForeignArray (`c_recip_multiply__m128d_a` coerce x) (x /) perAlignment
{-# INLINE recipMultiplyForeignArray #-}

absForeignArray :: Sz1 -> ForeignArray Double -> ForeignArray Double -> IO ()
absForeignArray = liftAlignedForeignArray c_abs__m128d_a abs perAlignment
{-# INLINE absForeignArray #-}

additionForeignArray ::
     Sz1 -> ForeignArray Double -> ForeignArray Double -> ForeignArray Double -> IO ()
additionForeignArray = zipWithAlignedForeignArray c_addition__m128d_a (+) perAlignment
{-# INLINE additionForeignArray #-}

subtractionForeignArray ::
     Sz1 -> ForeignArray Double -> ForeignArray Double -> ForeignArray Double -> IO ()
subtractionForeignArray = zipWithAlignedForeignArray c_subtraction__m128d_a (-) perAlignment
{-# INLINE subtractionForeignArray #-}


multiplicationForeignArray ::
     Sz1 -> ForeignArray Double -> ForeignArray Double -> ForeignArray Double -> IO ()
multiplicationForeignArray = zipWithAlignedForeignArray c_multiplication__m128d_a (*) perAlignment
{-# INLINE multiplicationForeignArray #-}

divisionForeignArray ::
     Sz1 -> ForeignArray Double -> ForeignArray Double -> ForeignArray Double -> IO ()
divisionForeignArray = zipWithAlignedForeignArray c_division__m128d_a (/) perAlignment
{-# INLINE divisionForeignArray #-}

sqrtForeignArray :: Sz1 -> ForeignArray Double -> ForeignArray Double -> IO ()
sqrtForeignArray = liftAlignedForeignArray c_sqrt__m128d_a sqrt perAlignment
{-# INLINE sqrtForeignArray #-}

truncateForeignArray :: Sz1 -> ForeignArray Double -> ForeignArray Int64 -> IO ()
truncateForeignArray =
  liftAlignedForeignArray
    c_truncate_64i__m128d_a
    (fromIntegral . double2Int) -- FIXME: 32bit machine is a problem
    perAlignment
{-# INLINE truncateForeignArray #-}

-- roundForeignArray :: Sz1 -> ForeignArray Double -> ForeignArray Double -> IO ()
-- roundForeignArray = liftAlignedForeignArray c_round__m128d_a roundDouble perAlignment
-- {-# INLINE roundForeignArray #-}


sumForeignArray :: Sz1 -> ForeignArray Double -> IO Double
sumForeignArray = foldWithAlignedForeignArray c_sum__m128d_a (+) 0 perAlignment
{-# INLINE sumForeignArray #-}

productForeignArray :: Sz1 -> ForeignArray Double -> IO Double
productForeignArray = foldWithAlignedForeignArray c_product__m128d_a (*) 1 perAlignment
{-# INLINE productForeignArray #-}

evenPowerSumForeignArray :: Sz1 -> ForeignArray Double -> Int -> IO Double
evenPowerSumForeignArray = evenPowerSumAlignedForeignArray c_even_power_sum__m128d_a perAlignment
{-# INLINE evenPowerSumForeignArray #-}

absPowerSumForeignArray :: Sz1 -> ForeignArray Double -> Int -> IO Double
absPowerSumForeignArray = absPowerSumAlignedForeignArray c_abs_power_sum__m128d_a perAlignment
{-# INLINE absPowerSumForeignArray #-}

absMaxForeignArray :: Sz1 -> ForeignArray Double -> IO Double
absMaxForeignArray = absMaxAlignedForeignArray c_abs_max__m128d_a perAlignment
{-# INLINE absMaxForeignArray #-}

maximumForeignArray :: Double -> Sz1 -> ForeignArray Double -> IO Double
maximumForeignArray e0 = foldWithAlignedForeignArray c_maximum__m128d_a max e0 perAlignment
{-# INLINE maximumForeignArray #-}

minimumForeignArray :: Double -> Sz1 -> ForeignArray Double -> IO Double
minimumForeignArray e0 = foldWithAlignedForeignArray c_minimum__m128d_a min e0 perAlignment
{-# INLINE minimumForeignArray #-}

copyForeignArray :: Sz1 -> ForeignArray Double -> ForeignArray Double -> IO ()
copyForeignArray = copyWithAlignedForeignArray c_copy__m128d_a perAlignment
{-# INLINE copyForeignArray #-}

fillForeignArray :: Sz1 -> ForeignArray Double -> Double -> IO ()
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

foreign import ccall unsafe "m128d.c massiv_negate_plus__m128d_a"
  c_negate_plus__m128d_a :: Ptr CDouble -> CDouble -> Ptr CDouble -> CLong -> IO ()

foreign import ccall unsafe "m128d.c massiv_multiply__m128d_a"
  c_multiply__m128d_a :: Ptr CDouble -> CDouble -> Ptr CDouble -> CLong -> IO ()

foreign import ccall unsafe "m128d.c massiv_power__m128d_a"
  c_power__m128d_a :: Ptr CDouble -> CLong -> Ptr CDouble -> CLong -> IO ()

foreign import ccall unsafe "m128d.c massiv_recip_power__m128d_a"
  c_recip_power__m128d_a :: Ptr CDouble -> CLong -> Ptr CDouble -> CLong -> IO ()

foreign import ccall unsafe "m128d.c massiv_abs__m128d_a"
  c_abs__m128d_a :: Ptr CDouble -> Ptr CDouble -> CLong -> IO ()

foreign import ccall unsafe "m128d.c massiv_addition__m128d_a"
  c_addition__m128d_a :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CLong -> IO ()

foreign import ccall unsafe "m128d.c massiv_subtraction__m128d_a"
  c_subtraction__m128d_a :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CLong -> IO ()

foreign import ccall unsafe "m128d.c massiv_multiplication__m128d_a"
  c_multiplication__m128d_a :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CLong -> IO ()

foreign import ccall unsafe "m128d.c massiv_division__m128d_a"
  c_division__m128d_a :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CLong -> IO ()

foreign import ccall unsafe "m128d.c massiv_divide__m128d_a"
  c_divide__m128d_a :: Ptr CDouble -> CDouble -> Ptr CDouble -> CLong -> IO ()

foreign import ccall unsafe "m128d.c massiv_recip_multiply__m128d_a"
  c_recip_multiply__m128d_a :: Ptr CDouble -> CDouble -> Ptr CDouble -> CLong -> IO ()

foreign import ccall unsafe "m128d.c massiv_sqrt__m128d_a"
  c_sqrt__m128d_a :: Ptr CDouble -> Ptr CDouble -> CLong -> IO ()


foreign import ccall unsafe "m128d.c massiv_truncate_64i__m128d_a"
  c_truncate_64i__m128d_a :: Ptr CDouble -> Ptr CLLong -> CLong -> IO ()

foreign import ccall unsafe "m128d.c massiv_truncate__m128d_a"
  c_truncate__m128d_a :: Ptr CDouble -> Ptr CLLong -> CLong -> IO ()

foreign import ccall unsafe "m128d.c massiv_round__m128d_a"
  c_round__m128d_a :: Ptr CDouble -> Ptr CDouble -> CLong -> IO ()



foreign import ccall unsafe "m128d.c massiv_sum__m128d_a"
  c_sum__m128d_a :: CDouble -> Ptr CDouble -> CLong -> IO CDouble

foreign import ccall unsafe "m128d.c massiv_product__m128d_a"
  c_product__m128d_a :: CDouble -> Ptr CDouble -> CLong -> IO CDouble

foreign import ccall unsafe "m128d.c massiv_even_power_sum__m128d_a"
  c_even_power_sum__m128d_a :: CLong -> CDouble -> Ptr CDouble -> CLong -> IO CDouble

foreign import ccall unsafe "m128d.c massiv_abs_power_sum__m128d_a"
  c_abs_power_sum__m128d_a :: CLong -> CDouble -> Ptr CDouble -> CLong -> IO CDouble

foreign import ccall unsafe "m128d.c massiv_abs_max__m128d_a"
  c_abs_max__m128d_a :: CDouble -> Ptr CDouble -> CLong -> IO CDouble


foreign import ccall unsafe "m128d.c massiv_maximum__m128d_a"
  c_maximum__m128d_a :: CDouble -> Ptr CDouble -> CLong -> IO CDouble

foreign import ccall unsafe "m128d.c massiv_minimum__m128d_a"
  c_minimum__m128d_a :: CDouble -> Ptr CDouble -> CLong -> IO CDouble

