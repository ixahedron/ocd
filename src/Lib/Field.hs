module Lib.Field (
                   Fp2Elem
                 , Moddable(..)
                 , Fieldish(..)
                 , (≡)
                 , inv
                 , mexp
                 , euc
                 , eGCD
                 , jacobi
                 , shankBG
                 ) where

import Data.List (sort)
import Data.Maybe (fromMaybe)

type Fp2Elem = (Integer,Integer)

instance Num Fp2Elem where
  negate (x,i) = (-x,-i)
  (x,i)+(w,j)  = (x+w,i+j)
  (x,i)*(w,j)  = (x*w-i*j,x*j+w*i)
  fromInteger  = (,0)
  abs (x,i)    = (abs x, abs i) -- this is probably wrong, fix?
  signum (x,i) = (signum x, signum i)

class (Eq p, Num p) => Moddable p where
  pmod :: Integral n => p -> n -> p
instance Moddable Fp2Elem where
  pmod (x,i) (fromIntegral -> p) = (x `mod` p, i `mod` p)
instance Moddable Integer where
  pmod a (fromIntegral -> p) = mod a p

class (Ord p, Num p, Moddable p) => Fieldish p where
  pinv :: Integral n => p -> n -> p
  toP2 :: p -> Fp2Elem
  zero :: p
  zero = 0

instance Fieldish Integer where
  pinv (fromInteger -> a) (fromIntegral -> p) = inv a p
  toP2 = fromInteger :: Integer -> Fp2Elem

instance Fieldish Fp2Elem where
  pinv (x,i) (fromIntegral -> p) = ((x,-i) * fromInteger (inv (x*x+i*i) p)) `pmod` p
  toP2 = id

infix 5 ≡
(≡) :: (Moddable a, Integral n) => a -> a -> n -> Bool
a ≡ b = \p -> (a `pmod` p) == (b `pmod` p)

-- multiplicative inverse using EEA
inv :: (Integral n, Show n) => n -> n -> n
inv a p = if gcd a p == 1
          then fst (euc a p) `mod` p
          else error $ show p ++ " can't compute inverse if gcd =/= 1 " ++ show a

-- modular (fast) exponentiation using binary method
-- mexp m a e = a^e (mod m)
mexp :: (Integral n, Fieldish a) => n -> a -> n -> a
mexp 1 _ _ = 0
mexp m x y = raise x y
  where raise _    0 = 1
        raise a    1 = a `pmod` m
        raise a e | a == zero = zero
                  | e < 0  = raise (pinv a m) (-e)
                  | otherwise = let t = if odd e then a `pmod` m else 1
                    in (t * raise ((a*a) `pmod` m) (e `div` 2)) `pmod` m

-- euc a b = (x,y) => ax + by = gcd a b
euc :: Integral n => n -> n -> (n,n)
euc a b | a `mod` b == 0 = (0,1)
        | otherwise = (y,x-y*(a `div` b))
  where (x,y) = euc b $ a `mod` b

-- same as euc but returning (x,y,gcd a b)
eGCD :: Integral n => n -> n -> (n,n,n)
eGCD a b | mod a b == 0 = (0,1,b)
         | otherwise = (y,x-y*(a `div` b),z)
        where
          (x,y,z) = eGCD b $ a `mod` b

-- Jacobi symbol
jacobi :: Integral n => n -> n -> n
jacobi (-1) b | b `mod` 4 == 1 = 1
              | b `mod` 4 == 3 = -1
              | otherwise = error "even b"
jacobi   2  b | (b `mod` 8) `elem` [1,7] = 1
              | (b `mod` 8) `elem` [3,5] = -1
              | otherwise = error "even b"
jacobi   a  b | a `mod` b == 0 = 0
              | a `mod` b == b-1 = jacobi (-1) b
              | a `mod` b == 2   = jacobi 2 b
              | even a = let (k,p) = reduce 0 a;
                             q = if even k then 1 else jacobi 2 b
                         in q * jacobi p b
              | odd b = case (a `mod` 4, b `mod` 4) of
                          (3,3) -> jacobi (-1) a * jacobi (b `mod` a) a
                          _ -> jacobi (b `mod` a) a
              | otherwise = error "even b"
  where reduce :: Integral n => n -> n -> (n,n)
        reduce k n | odd n = (k,n)
                   | otherwise = reduce (k+1) $ n `div` 2

-- Shank's babystep-giantstep algo for cracking DLP
-- NB: order calculation is inefficient as all hell
shankBG :: (Integral n, Fieldish a) => n -> n -> a -> a -> n
shankBG p order g h = i + j*n
  where n = 1 + flrt order
        --order = head . filter (\x -> mexp p g x == 1) $ [1..p]
        lg = sort [(mexp p g k, k) | k<-[0..n]]
        lh = sort [((h * pinv (mexp p g (n*k)) p) `pmod` p, k) | k<-[0..n]]
        (i,j) = fromMaybe (0,0) $ match lg lh

flrt :: Integral n => n -> n  -- flrt x ≈ √x,  with  flrt x² ≤ x < flrt(x+1)²
flrt = floor . sqrt . fromIntegral

match :: (Integral n, Fieldish a) => [(a,n)] -> [(a,n)] -> Maybe (n,n)
match [] _ = Nothing
match _ [] = Nothing
match xx@((x,i):xs) yy@((y,j):ys) | x == y = Just (i,j)
                                  | x > y = match xx ys
                                  | otherwise = match xs yy
