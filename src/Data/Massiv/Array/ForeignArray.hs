{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- |
-- Module      : Data.Massiv.Array.ForeignArray
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.ForeignArray
  ( module Data.Massiv.Array.ForeignArray.Internal
  -- * Operations on ForeignArray
  -- ** Fill and copy
  , fillWithForeignArray
  , copyWithForeignArray
  , fillWithAlignedForeignArray
  , copyWithAlignedForeignArray
  -- ** Folding
  , eqWithForeignArray
  , foldWithForeignArray
  , fold2WithForeignArray
  , eqWithAlignedForeignArray
  , foldWithAlignedForeignArray
  , fold2WithAlignedForeignArray
  , foldNonEmptyWithAlignedForeignArray
  -- ** Lifting
  , liftForeignArray
  , zipWithForeignArray
  , liftAlignedForeignArray
  , liftAlignedForeignArray'
  , zipWithAlignedForeignArray
  -- ** Numeric
  , evenPowerSumAlignedForeignArray
  , absPowerSumAlignedForeignArray
  , multiplySumAlignedForeignArray
  ) where

import Data.Coerce
import Data.Massiv.Array.ForeignArray.Internal
import Data.Massiv.Core.Index
import Foreign.C
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable


fillWithForeignArray ::
     (Coercible a b, Storable a)
  => (Ptr b -> CLong -> b -> IO e)
  -> ForeignArray ix a
  -> Ix1
  -> Sz1
  -> a
  -> IO e
fillWithForeignArray setAction arr offset sz e =
  withForeignArray arr $ \ptr ->
    setAction
      (coerce (advancePtr ptr offset))
      (fromIntegral (unSz sz))
      (coerce e)
{-# INLINE fillWithForeignArray #-}


copyWithForeignArray ::
     (Storable a, Coercible a b)
  => (Ptr b -> Ptr b -> CLong -> IO ())
  -> ForeignArray ix1 a
  -> Ix1
  -> ForeignArray ix2 a
  -> Ix1
  -> Sz1
  -> IO ()
copyWithForeignArray copyWith arrs iFrom arrd iTo (Sz k) =
  withForeignArray arrs $ \ps ->
    withForeignArray arrd $ \pd ->
      copyWith (coerce (advancePtr ps iFrom)) (coerce (advancePtr pd iTo)) (fromIntegral k)
{-# INLINE copyWithForeignArray #-}


foldWithForeignArray ::
     (Coercible a b, Index ix)
  => (Ptr b -> CLong -> IO e)
  -> ForeignArray ix a
  -> IO e
foldWithForeignArray foldAction arr =
  withForeignArray arr $ \ptr ->
    foldAction (coerce ptr) (fromIntegral (unSz (lengthForeignArray arr)))
{-# INLINE foldWithForeignArray #-}


fold2WithForeignArray ::
     (Coercible a b, Coercible e c, Index ix)
  => (Ptr b -> Ptr b -> CLong -> IO e)
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO c
fold2WithForeignArray foldAction arr1 arr2 =
  let k = fromIntegral (unSz (lengthForeignArray arr1))
   in coerce $
      withForeignArray arr1 $ \p1 ->
        withForeignArray arr2 $ \p2 -> foldAction (coerce p1) (coerce p2) k
{-# INLINE fold2WithForeignArray #-}


eqWithForeignArray ::
     (Coercible a b, Index ix)
  => (Ptr b -> Ptr b -> CLong -> IO CBool)
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO Bool
eqWithForeignArray eqAction arr1 arr2 = fold2WithForeignArray eqWithPtrs arr1 arr2
  where
    eqWithPtrs p1 p2 _
      | p1 == p2 = pure True
      | otherwise = cboolToBool <$> eqAction p1 p2 (fromIntegral (unSz (lengthForeignArray arr1)))
    {-# INLINE eqWithPtrs #-}
{-# INLINE eqWithForeignArray #-}


zipWithForeignArray ::
     (Storable a, Index ix)
  => (Ptr b -> Ptr b -> Ptr b -> CLong -> IO ())
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO ()
zipWithForeignArray zipWithAction arr1 arr2 resArr =
  withForeignArray arr1 $ \p1 ->
    withForeignArray arr2 $ \p2 ->
      withForeignArray resArr $ \pRes ->
        let sz = fromIntegral (unSz (lengthForeignArray resArr))
         in zipWithAction (coerce p1) (coerce p2) (coerce pRes) sz
{-# INLINE zipWithForeignArray #-}

liftForeignArray ::
     (Storable a, Index ix)
  => (Ptr b -> Ptr b -> CLong -> IO ())
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO ()
liftForeignArray liftAction arr resArr =
  withForeignArray arr $ \p ->
    withForeignArray resArr $ \pRes ->
      let sz = fromIntegral (unSz (lengthForeignArray resArr))
       in liftAction (coerce p) (coerce pRes) sz
{-# INLINE liftForeignArray #-}



cboolToBool :: CBool -> Bool
cboolToBool = (/= 0)
{-# INLINE cboolToBool #-}



-- Arguments to the supplied action, eg. 4 elements per alignment:
--
--                          .- ptr (ForeignArray start)
--  beginning of           /    .- ptrAlignedAdjusted
--   ForeignPtr \         |    /                     .-- ForeignArray end
--               v        v   v                     /
--               x.x.x.x.[x.x|x.x.x.x|x.x.x.x|x.x.x].x.x.x.x.x <- end of allocated memory
--                       |\ / \___________   \      \____________
--                       | v              \___\                  \
--                       | lengthBefore:2      `lengthAligned:8   \
--                       \_________________________________________\
--                                                                  `lengthTotal:13
withAlignedForeignArray ::
     forall a c ix. (Storable a)
  => ForeignArray ix a
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> Sz1
  -> (Ptr a -> Int -> Ptr a -> Int -> IO c)
  -> IO c
withAlignedForeignArray arr perAlignment (Sz lengthTotal) action =
  withForeignArray arr $ \ptr -> do
    let !esize = sizeOf (undefined :: a)
        !ptrAligned = alignPtr ptr (perAlignment * esize)
        !lengthBefore = min lengthTotal ((ptrAligned `minusPtr` ptr) `div` esize)
        !lengthAligned = ((lengthTotal - lengthBefore) `div` perAlignment) * perAlignment
        !ptrAlignedAdjusted = ptr `advancePtr` lengthBefore
    action ptr lengthBefore ptrAlignedAdjusted lengthAligned
{-# INLINE withAlignedForeignArray #-}

applyAlignedForeignArray ::
     (Storable a, Index ix, Coercible a b, Coercible e c)
  => (e -> Ptr b -> CLong -> IO e)
  -> (c -> Ptr a -> IO c)
  -> c
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> IO c
applyAlignedForeignArray applyAligned apply initAcc perAlignment arr = do
  let sz = lengthForeignArray arr
      lengthTotal = unSz sz
  withAlignedForeignArray arr perAlignment sz $ \ptr lengthBefore ptrAligned lengthAligned -> do
    let applyLoop from to iAcc =
          loopM from (< to) (+ 1) iAcc $ \i acc -> apply acc (ptr `advancePtr` i)
        {-# INLINE applyLoop #-}
        ptrAdjusted = coerce ptrAligned
    beforeRes <- applyLoop 0 lengthBefore initAcc
    resAligned <- applyAligned (coerce beforeRes) ptrAdjusted (fromIntegral lengthAligned)
    applyLoop (lengthBefore + lengthAligned) lengthTotal (coerce resAligned)
{-# INLINE applyAlignedForeignArray #-}

apply2AlignedForeignArray ::
     (Storable a, Index ix, Coercible a b, Coercible e c)
  => (e -> Ptr b -> Ptr b -> CLong -> IO e)
  -> (c -> Ptr a -> Ptr a -> IO c)
  -> c
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO c
apply2AlignedForeignArray applyAligned apply initAcc perAlignment arr1 arr2 = do
  let sz = lengthForeignArray arr1
      lengthTotal = unSz sz
  withAlignedForeignArray arr1 perAlignment sz $ \ptr1 lengthBefore ptr1Aligned lengthAligned ->
    withForeignArray arr2 $ \ptr2 -> do
      let applyLoop from to iAcc =
            loopM from (< to) (+ 1) iAcc $ \i acc ->
              apply acc (ptr1 `advancePtr` i) (ptr2 `advancePtr` i)
          {-# INLINE applyLoop #-}
          ptr1Adjusted = coerce ptr1Aligned
          ptr2Adjusted = coerce (ptr2 `advancePtr` lengthBefore)
      resBefore <- applyLoop 0 lengthBefore initAcc
      resAligned <-
        applyAligned (coerce resBefore) ptr1Adjusted ptr2Adjusted (fromIntegral lengthAligned)
      applyLoop (lengthBefore + lengthAligned) lengthTotal (coerce resAligned)
{-# INLINE apply2AlignedForeignArray #-}


apply2AlignedForeignArray' ::
     (Storable a, Storable b, Index ix, Coercible a x, Coercible b y, Coercible e c)
  => (e -> Ptr x -> Ptr y -> CLong -> IO e)
  -> (c -> Ptr a -> Ptr b -> IO c)
  -> c
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> ForeignArray ix b
  -> IO c
apply2AlignedForeignArray' applyAligned apply initAcc perAlignment arr1 arr2 = do
  let sz = lengthForeignArray arr1
      lengthTotal = unSz sz
  withAlignedForeignArray arr1 perAlignment sz $ \ptr1 lengthBefore ptr1Aligned lengthAligned ->
    withForeignArray arr2 $ \ptr2 -> do
      let applyLoop from to iAcc =
            loopM from (< to) (+ 1) iAcc $ \i acc ->
              apply acc (ptr1 `advancePtr` i) (ptr2 `advancePtr` i)
          {-# INLINE applyLoop #-}
          ptr1Adjusted = coerce ptr1Aligned
          ptr2Adjusted = coerce (ptr2 `advancePtr` lengthBefore)
      resBefore <- applyLoop 0 lengthBefore initAcc
      resAligned <-
        applyAligned (coerce resBefore) ptr1Adjusted ptr2Adjusted (fromIntegral lengthAligned)
      applyLoop (lengthBefore + lengthAligned) lengthTotal (coerce resAligned)
{-# INLINE apply2AlignedForeignArray' #-}

apply3AlignedForeignArray ::
     (Storable a, Index ix, Coercible a b, Coercible e c)
  => (e -> Ptr b -> Ptr b -> Ptr b -> CLong -> IO e)
  -> (c -> Ptr a -> Ptr a -> Ptr a -> IO c)
  -> c
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO c
apply3AlignedForeignArray applyAligned apply initAcc perAlignment arr1 arr2 arr3 = do
  let sz = lengthForeignArray arr1
      lengthTotal = unSz sz
  withAlignedForeignArray arr1 perAlignment sz $ \ptr1 lengthBefore ptr1Aligned lengthAligned ->
    withForeignArray arr2 $ \ptr2 ->
      withForeignArray arr3 $ \ptr3 -> do
        let applyLoop from to iAcc =
              loopM from (< to) (+ 1) iAcc $ \i acc ->
                apply acc (ptr1 `advancePtr` i) (ptr2 `advancePtr` i) (ptr3 `advancePtr` i)
            {-# INLINE applyLoop #-}
            ptr1Adjusted = coerce ptr1Aligned
            ptr2Adjusted = coerce (ptr2 `advancePtr` lengthBefore)
            ptr3Adjusted = coerce (ptr3 `advancePtr` lengthBefore)
            clengthAligned = fromIntegral lengthAligned
        resBefore <- applyLoop 0 lengthBefore initAcc
        resAligned <-
          applyAligned (coerce resBefore) ptr1Adjusted ptr2Adjusted ptr3Adjusted clengthAligned
        applyLoop (lengthBefore + lengthAligned) lengthTotal (coerce resAligned)
{-# INLINE apply3AlignedForeignArray #-}


liftAlignedForeignArray ::
     (Storable a, Index ix, Coercible a b)
  => (Ptr b -> Ptr b -> CLong -> IO ())
  -> (a -> a)
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO ()
liftAlignedForeignArray liftAligned f =
  apply2AlignedForeignArray (const liftAligned) (\_ p1 p2 -> poke p2 . f =<< peek p1) ()
{-# INLINE liftAlignedForeignArray #-}

liftAlignedForeignArray' ::
     (Storable a, Storable b, Index ix, Coercible a x, Coercible b y)
  => (Ptr x -> Ptr y -> CLong -> IO ())
  -> (a -> b)
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> ForeignArray ix b
  -> IO ()
liftAlignedForeignArray' liftAligned f =
  apply2AlignedForeignArray' (const liftAligned) (\_ p1 p2 -> poke p2 . f =<< peek p1) ()
{-# INLINE liftAlignedForeignArray' #-}


zipWithAlignedForeignArray ::
     (Storable a, Index ix, Coercible a b, Show a, Num a, Eq a)
  => (Ptr b -> Ptr b -> Ptr b -> CLong -> IO ())
  -> (a -> a -> a)
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO ()
zipWithAlignedForeignArray zipWithAligned f =
  apply3AlignedForeignArray
    (const zipWithAligned)
    (\_ p1 p2 p3 -> f <$> peek p1 <*> peek p2 >>= poke p3)
    ()
{-# INLINE zipWithAlignedForeignArray #-}


foldWithAlignedForeignArray ::
     (Storable a, Coercible a b, Coercible e c, Index ix)
  => (e -> Ptr b -> CLong -> IO e)
  -> (c -> a -> c)
  -> c
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> IO c
foldWithAlignedForeignArray foldAligned foldUnaligned =
  applyAlignedForeignArray foldAligned (\ !acc !ptr -> foldUnaligned acc <$> peek ptr)
{-# INLINE foldWithAlignedForeignArray #-}


fold2WithAlignedForeignArray ::
     (Storable a, Coercible a b, Coercible e c, Index ix)
  => (e -> Ptr b -> Ptr b -> CLong -> IO e)
  -- ^ Folding SIMD action. Ensure that the first pointer is always aligned according to
  -- the supplied alignment
  -> (c -> a -> a -> c)
  -> c
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO c
fold2WithAlignedForeignArray foldAligned foldUnaligned =
  apply2AlignedForeignArray foldAligned (\ !acc !p1 !p2 -> foldUnaligned acc <$> peek p1 <*> peek p2)
{-# INLINE fold2WithAlignedForeignArray #-}


foldNonEmptyWithAlignedForeignArray ::
     (Storable a, Coercible a b, Coercible e a, Index ix)
  => (e -> Ptr b -> CLong -> IO e)
  -> (a -> a -> a)
  -> Int -- ^ Alignment. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> IO a
foldNonEmptyWithAlignedForeignArray foldAligned foldUnaligned perAlignment arr = do
  e0 <- readForeignArray arr 0
  let arrNo0 = extractForeignArray 1 (lengthForeignArray arr - 1) arr
  foldWithAlignedForeignArray foldAligned foldUnaligned e0 perAlignment arrNo0
{-# INLINE foldNonEmptyWithAlignedForeignArray #-}


eqWithAlignedForeignArray ::
     (Eq a, Storable a, Coercible a b, Index ix)
  => (Ptr b -> Ptr b -> CLong -> IO CBool)
  -> Int -- ^ Alignement. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO Bool
eqWithAlignedForeignArray eqAction =
  fold2WithAlignedForeignArray eqWithPtrs (\acc x y -> acc && x == y) True
  where
    eqWithPtrs acc p1 p2 sz
      | p1 == p2 = pure True -- arrays are same whenever two pointers are equal, even when
                             -- they are adjusted for alignement
      | not acc = pure False
      | otherwise = cboolToBool <$> eqAction p1 p2 sz
    {-# INLINE eqWithPtrs #-}
{-# INLINE eqWithAlignedForeignArray #-}



fillWithAlignedForeignArray ::
     (Index ix, Coercible a b, Storable a)
  => (b -> Ptr b -> CLong -> IO ())
  -> Int -- ^ Alignement. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> a
  -> IO ()
fillWithAlignedForeignArray fillAction perAlignment arr e =
  applyAlignedForeignArray (\_ -> fillAction (coerce e)) (\_ p -> poke p e) () perAlignment arr
{-# INLINE fillWithAlignedForeignArray #-}


copyWithAlignedForeignArray ::
     (Storable a, Coercible a b, Index ix)
  => (Ptr b -> Ptr b -> CLong -> IO ())
  -> Int -- ^ Alignement. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO ()
copyWithAlignedForeignArray copyAction =
  apply2AlignedForeignArray (const copyAction) (\ _ ptr1 ptr2 -> peek ptr1 >>= poke ptr2) ()
{-# INLINE copyWithAlignedForeignArray #-}

evenPowerSumAlignedForeignArray ::
     (Storable c, Index ix, Num c, Coercible e c)
  => (CLong -> e -> Ptr e -> CLong -> IO e)
  -> Int -- ^ Alignement. In number of elements, rather than bytes.
  -> ForeignArray ix c
  -> Int -- ^ Power
  -> IO c
evenPowerSumAlignedForeignArray action perAlignment arr pow =
  foldWithAlignedForeignArray (action (fromIntegral pow)) (powerSum pow) 0 perAlignment arr
{-# INLINE evenPowerSumAlignedForeignArray #-}

absPowerSumAlignedForeignArray ::
     (Storable c, Index ix, Num c, Coercible e c)
  => (CLong -> e -> Ptr e -> CLong -> IO e)
  -> Int -- ^ Alignement. In number of elements, rather than bytes.
  -> ForeignArray ix c
  -> Int -- ^ AbsPower
  -> IO c
absPowerSumAlignedForeignArray action perAlignment arr pow =
  foldWithAlignedForeignArray
    (action (fromIntegral pow))
    (\acc -> powerSum pow acc . abs)
    0
    perAlignment
    arr
{-# INLINE absPowerSumAlignedForeignArray #-}

powerSum :: (Integral b, Num a) => b -> a -> a -> a
powerSum pow acc x = acc + x ^ pow
{-# INLINE powerSum #-}

multiplySumAlignedForeignArray ::
     (Storable a, Index ix, Num a, Coercible a b, Coercible e a)
  => (e -> Ptr b -> Ptr b -> CLong -> IO e)
  -> Int -- ^ Alignement. In number of elements, rather than bytes.
  -> ForeignArray ix a
  -> ForeignArray ix a
  -> IO a
multiplySumAlignedForeignArray multiplySumAligned =
  fold2WithAlignedForeignArray multiplySumAligned (\acc x y -> acc + x * y) 0
{-# INLINE multiplySumAlignedForeignArray #-}
