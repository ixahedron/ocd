{-# LANGUAGE DeriveFunctor #-}

module Lib.Matrix where

import Control.Arrow ((&&&), (***))
import Data.List (unzip, transpose, intercalate)

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
  show (dematrix -> m) = intercalate "\n" . map show $ m

matrix :: [[a]] -> Matrix a
matrix xs = let m = fromIntegral $ length xs
                n = fromIntegral $ length (head xs)
            in if length (filter (\x -> fromIntegral (length x) == n) xs) == length xs
               then Matrix . map Vect $ xs
               else error "Constructing matrix: incorrect dimension"

dematrix :: Matrix a -> [[a]]
dematrix = map vlist . rows

mij :: Integral n => Matrix a -> n -> n -> a
mij m i j | i > fst (mdim m) || j > snd (mdim m) = error "indices too large"
mij (rows -> m) (fromIntegral -> i) (fromIntegral -> j) = vlist (m !! (i-1)) !! (j-1)

mi :: Integral n => Matrix a -> n -> Vect a
mi (rows -> m) (fromIntegral -> i) | i > length m = error "index too large"
                                   | otherwise = m !! (i-1)

mj :: Integral n => Matrix a -> n -> Vect a
mj m = mi (mtranspose m)

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
              if n1 /= m2 then error "matrices of non-multipliable dimensions"
                          else let bc' = dematrix . mtranspose $ b;
                                   ar' = dematrix a
                               in Matrix . map Vect $ [ [ sum $ zipWith (*) ar bc | bc <- bc'] | ar <- ar' ]

minverse :: (Eq a, Fractional a) => Matrix a -> Matrix a
minverse (mdet -> 0) = error "this matrix is non-invertible"
minverse m = (1 / mdet m) `mscal` madjoint m

mswap :: Integral n => Matrix a -> n -> n -> Matrix a
mswap _ i j | i < 1 || j < 1 = error "mswap: index less than 1"
mswap m i j | let (k,_) = mdim m in i > k || j > k = error "mswap: index too large"
            | j < i = mswap m j i
mswap (rows -> m) (fromIntegral -> i) (fromIntegral -> j)
    = Matrix $ take (i-1) m ++ (m !! (j-1)) : take (j-i-1) (drop i m) ++ (m !! (i-1)) : drop j m

mtranspose :: Matrix a -> Matrix a
mtranspose = matrix . transpose . dematrix

mdet :: Num a => Matrix a -> a
mdet (rows -> []) = 0
mdet m@(mdim -> (1,1)) = mij m 1 1
mdet m@(rows -> rs) | k /= l = error "determinant is for square matrices only"
                    | otherwise = sum [(-1)^(j+1) * mij m 1 j * mdet (mAij m 1 j) | j<-[1..l]]
  where (k,l) = mdim m

-- remove the ith row and the jth column from the given matrix
-- for calculating minors
mAij :: Integral n => Matrix a -> n -> n -> Matrix a
mAij (rows -> m) i j = matrix . dropIth i . map (dropIth j . vlist) $ m
  where dropIth :: Integral n => n -> [a] -> [a]
        dropIth (fromIntegral -> i) xs = take (i-1) xs ++ drop i xs

-- lol mad joint kek
madjoint :: Num a => Matrix a -> Matrix a
madjoint m@(mdim -> (k,l)) = Matrix . map Vect $
      map (\i -> map (f i) [1..k]) [1..l]
  where f i j = (-1)^((i+j) `mod` 2) * mdet (mAij m j i)

-- combine A and b into a matrix w/ the last column b
mvappend :: Matrix a -> Vect a -> Matrix a
mvappend m v = matrix [r ++ [x]| (r,x) <- zip (dematrix m) (vlist v) ]

----------------------------------------------------------

-- Gauss-Jordan elimination: solve Ax = b
gaussElimination :: (Fractional a, Eq a) => Matrix a -> Vect a -> Vect a
gaussElimination a b = snd . jordan . rowEchelon $ mvappend a b


-- Transform every element above the diagonal to zero, i.e. compute the reduced
-- echelon form of a matrix, given that the input is in row echelon form.
jordan :: Num a => (Matrix a, Vect a) -> (Matrix a, Vect a)
jordan (m, Vect ys) = let (a,b) = unzip $ jordan' (zip (dematrix m) ys) (k-1)
                      in (matrix a, Vect b)
  where (k,_) = mdim m

        jordan' [] _ = []
        jordan' xs c = jordan' [ (take c x ++ 0 : drop (c+1) x, v - x !! c * snd (last xs))
                               | (x,v) <- init xs ] (c-1) ++ [last xs]

-- compute row echelon form of a system Ax = b
rowEchelon :: (Fractional a, Eq a) => Matrix a -> (Matrix a, Vect a)
rowEchelon aug = re_aux aug 1 1
  where
  -- i and j are the current row and column
  re_aux (rows -> []) _ _ = error "rowEchelon: Empty input matrix"
  re_aux m (fromIntegral -> i) (fromIntegral -> j)
    | j == (l+1) || i == (k+1) = (matrix *** Vect) . unzip . map (init &&& last) . dematrix $ m
    | mij m i j == 0   = case findPivot m i j of
      Just (_,i') -> re_aux (mswap m i i') i j
      Nothing     -> re_aux m i (j+1)
    | mij m i j /= 1 = re_aux (rowScale m i (1 / mij m i j)) i j
    | otherwise          = case findPivot m i j of
      -- Make the first nonzero element in the pivot row 0.
      Just (v,i') -> re_aux (pivot m (-v) i i') i j
      Nothing     -> re_aux m (i+1) (j+1)

  rowScale (rows -> m) i n = Matrix $ take (i-1) m ++ (vscal n (m !! (i-1)) : drop i m)

  (k,l) = let (m,n) = mdim aug in (m,n-1)


pivot :: (Num a, Integral n) => Matrix a -> a -> n -> n -> Matrix a
pivot m s i j = addTo m j $ vscal s (mi m i)
  where addTo m (fromIntegral -> i) v = let m' = rows m
                                        in Matrix $ take (i-1) m' ++ (v `vplus` mi m i) : drop i m'

findPivot :: (Num a, Eq a, Integral n) => Matrix a -> n -> n -> Maybe (a,n)
findPivot m (fromIntegral -> i) (fromIntegral -> j) =
  safeHead . filter ((/= 0) . fst) . drop i $
    zip (head . drop (j-1) . dematrix . mtranspose $ m) [1..]
  where safeHead []     = Nothing
        safeHead (x:xs) = Just x
