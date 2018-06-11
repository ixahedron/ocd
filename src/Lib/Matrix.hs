{-# LANGUAGE DeriveFunctor #-}

module Lib.Matrix where

import Data.List (transpose, intercalate)

newtype Vect a = Vect { vlist :: [a] } deriving (Show, Eq, Ord, Functor)
newtype Matrix a = Matrix { rows :: [Vect a] } deriving (Eq)

---------------------------------------------------------------

vi :: Integral n => Vect a -> n -> a
vi (vlist -> v) (fromIntegral -> i) | i > length v = error "index too large"
                                    | otherwise = v !! (i-1)

vdim :: Integral n => Vect a -> n
vdim (vlist -> v) = fromIntegral $ length v

vplus :: Num a => Vect a -> Vect a -> Vect a
vplus (Vect a) (Vect b) = Vect $ zipWith (+) a b

vminus :: Num a => Vect a -> Vect a -> Vect a
vminus (Vect a) (Vect b) = Vect $ zipWith (-) a b

vnegate :: Num a => Vect a -> Vect a
vnegate = fmap negate

vscal :: Num a => a -> Vect a -> Vect a
vscal k = fmap (*k)

dot :: Num a => Vect a -> Vect a -> a
dot (vlist -> v) (vlist -> w) = sum $ zipWith (*) v w

vlength :: Floating a => Vect a -> a
vlength v = sqrt $ dot v v

vangle :: Floating a => Vect a -> Vect a -> a
vangle v w = let θ = vw / (vv * ww);
                 vw = dot v w;
                 vv = vlength v;
                 ww = vlength w
             in acos θ

---------------------------------------------------------------

instance Show a => Show (Matrix a) where
  show (map vlist . rows -> m) = intercalate "\n" . map show $ m

mij :: Integral n => Matrix a -> n -> n -> a
mij m i j | i > fst (mdim m) || j > snd (mdim m) = error "indices too large"
mij (rows -> m) (fromIntegral -> i) (fromIntegral -> j) = vlist (m !! (i-1)) !! (j-1)

mi :: Integral n => Matrix a -> n -> Vect a
mi (rows -> m) (fromIntegral -> i) | i > length m = error "index too large"
                                   | otherwise = m !! (i-1)

mdim :: Integral n => Matrix a -> (n,n)
mdim (rows -> []) = (0,0)
mdim (rows -> m) = let Vect n = head m in (fromIntegral $ length m, fromIntegral $ length n)

mscal :: Num a => a -> Matrix a -> Matrix a
mscal k (Matrix m) = Matrix $ map (vscal k) m

mplus :: Num a => Matrix a -> Matrix a -> Matrix a
mplus a b | mdim a /= mdim b = error "matrices must have the same dimensions for summation"
          | otherwise = Matrix $ zipWith vplus (rows a) (rows b)

mmult :: Num a => Matrix a -> Matrix a -> Matrix a
mmult a b = let (m1,n1) = mdim a; (m2,n2) = mdim b in
              if m1 /= n2 then error "matrices of non-multipliable dimensions"
                          else let bc' = map vlist . rows . mtranspose $ b;
                                   ar' = map vlist . rows $ a
                               in Matrix . map Vect $ [ [ sum $ zipWith (*) ar bc | bc <- bc'] | ar <- ar' ]

minverse :: (Eq a, Fractional a) => Matrix a -> Matrix a
minverse (mdet -> 0) = error "this matrix is non-invertible"
minverse m = (1 / mdet m) `mscal` madjoint m

mtranspose :: Matrix a -> Matrix a
mtranspose m = let m' = map vlist . rows $ m in Matrix . map Vect $ transpose m'

-- remove the ith row and the jth column from the given matrix
-- for calculating minors
mAij :: Integral n => Matrix a -> n -> n -> Matrix a
mAij (rows -> m) i j = (Matrix . map Vect) . dropIth i . map (dropIth j . vlist) $ m
  where dropIth :: Integral n => n -> [a] -> [a]
        dropIth (fromIntegral -> i) xs = take (i-1) xs ++ drop i xs

mdet :: Num a => Matrix a -> a
mdet (rows -> []) = 0
mdet m@(mdim -> (1,1)) = mij m 1 1
mdet m@(rows -> rs) | k /= l = error "determinant is for square matrices only"
                    | otherwise = sum [(-1)^(j+1) * mij m 1 j * mdet (mAij m 1 j) | j<-[1..l]]
  where (k,l) = mdim m

-- lol mad joint kek
madjoint :: Num a => Matrix a -> Matrix a
madjoint m@(mdim -> (k,l)) = Matrix . map Vect $
      map (\i -> map (f i) [1..k]) [1..l]
  where f i j = (-1)^((i+j) `mod` 2) * mdet (mAij m j i)
