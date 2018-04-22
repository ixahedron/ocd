module Lib.Polynomial where

import Data.List (sortOn, groupBy)
import Data.Function (on)
import Lib (inv)

newtype Poly = P [(Integer, Integer)]
data PolyF = PolyF {p :: Integer, quot :: Poly, poly :: Poly}

over :: ([(Integer,Integer)] -> [(Integer,Integer)]) -> Poly -> Poly
f `over` (P p) = P $ f p

overF :: ([(Integer,Integer)] -> [(Integer,Integer)]) -> PolyF -> PolyF
f `overF` (PolyF p q poly) = PolyF p q $ f `over` poly

overPinF :: (Poly -> Poly) -> PolyF -> PolyF
f `overPinF` (PolyF p q poly) = PolyF p q $ f poly

instance Eq Poly where
  (over normalize -> P p1) == (over normalize -> P p2) = p1 == p2

instance Eq PolyF where
  (==) (nf -> PolyF p' q1 p1) (nf -> PolyF p'' q2 p2) = p' == p'' && q1 == q2 && p1 == p2

nf = overF normalize . asToFieldF

instance Show PolyF where
  show (PolyF p q poly) = show poly

instance Show Poly where
  show (P []) = []
  show (P [(0,_)])   = []
  show (P [(a,0)])   = (if a /= -1 then sgn a else []) ++ show a
  show (P [(a,1)])   = sgn a ++ (if abs a > 1 then show a else []) ++ "X"
  show (P [(1,x)])   = "+X^" ++ show x
  show (P [(a,x)])   = sgn a ++ (if abs a > 1 then show a else []) ++ "X^" ++ show x
  show (P ((a,x):p)) = show (P [(a,x)]) ++ show (P p)

sgn a | a >= 0    = "+"
      | a == -1   = "-"
      | otherwise = []

pprint :: Poly -> IO ()
pprint (P p) = putStrLn . show . P . reverse . (sortOn snd) $ p

add :: Poly -> Poly -> Poly
add p (P []) = p
add (P []) p = p
add (P p1) (P p2) = P . normalize $ (p1 ++ p2)

mult :: Poly -> Poly -> Poly
mult (P []) _ = P []
mult _ (P []) = P []
mult (P p1) (P p2) = P . normalize $ ((\(a,x) (b,y) -> (a*b, x+y)) <$> p1 <*> p2)

normalize :: [(Integer, Integer)] -> [(Integer, Integer)]
normalize = filter (\x -> fst x /= 0) . map (\m -> (sum . (map fst) $ m, snd . head $ m)) .
            (groupBy ((==) `on` snd)) . (sortOn (\x -> - snd x))

-- in case of a finite field, take all coefficients mod p
reduceCoefs :: Integer -> [(Integer,Integer)] -> [(Integer,Integer)]
reduceCoefs p ps = map (\(a,x) -> (a `mod` p,x)) ps

asToFieldF :: PolyF -> PolyF
asToFieldF poly = (reduceCoefs (p poly)) `overF` poly

instance Num PolyF where
  (PolyF p q a) + (PolyF p' q' b) = asToFieldF $ PolyF p q (a + b)

  -- here I take the second set of params as a h4ck: in case of multiplication by a scalar
  -- the first set is undefined. But it's a shitty solution. How to make a better one?
  (PolyF _ _ a) * (PolyF p' q' b) = overF normalize . asToFieldF . PolyF p' q' $ (a * b) `pmod` q'
  abs (PolyF p q a) = asToFieldF $ PolyF p q (abs a)
  signum (PolyF p q a) = asToFieldF $ PolyF p q (signum a)

  negate (PolyF p q a) = asToFieldF $ PolyF p q (negate a)

  fromInteger p = PolyF undefined undefined (P [(p,0)])
  -- this is reliably breaking the logic, how do I fix this?

instance Num Poly where
  (+) = add
  (*) = mult
  abs (P p) = P (map (\(a,x) -> (abs a, x)) p)
  signum (P p) = P (map (\(a,x) -> (signum a, 0)) p)

  negate (P []) = P []
  negate (P p) = P (map (\(a,x) -> (-a,x)) p)

  fromInteger 0 = P []
  fromInteger a = P [(a,0)]

pdiv :: Poly -> Poly -> (Poly, Poly)
pdiv a b | deg a < deg b = (P [], a)
         | otherwise     = pdiv_aux (P []) a
  where pdiv_aux k r | deg r < deg b = (k,r)
                     | otherwise     = pdiv_aux (k + t) (r - t*b)
          where t = P [(h, deg r - deg b)]
                h = (fst . highest $ r) `div` (fst . highest $ b)

pmod a = snd . pdiv a

pdivF :: PolyF -> PolyF -> (PolyF, PolyF)
pdivF (PolyF p q a) (PolyF p' q' b) | p /= p' || q /= q' = error "polynomials must be in the same field"
                                    | otherwise = let (k,r) = pdiv a b
                                                  in (asToFieldF $ PolyF p q k, asToFieldF $ PolyF p q r)

pmodF :: PolyF -> PolyF -> PolyF
pmodF a b = snd $ pdivF a b

pinv :: PolyF -> PolyF
pinv pf = let q = fieldSize pf in pf^(q-2)

fieldSize :: PolyF -> Integer
fieldSize PolyF{..} = p^(deg quot)

deg :: Poly -> Integer
deg (P []) = -1
deg      p = snd . highest $ p

highest :: Poly -> (Integer, Integer)
highest (P []) = undefined
highest (P p)  = last . (sortOn snd) . normalize $ p

numberOfNonZeroTerms :: Poly -> Integer
numberOfNonZeroTerms (P p) = toInteger . length . normalize $ p

listPolyFs :: (Poly -> PolyF) -> [PolyF]
listPolyFs pf = zero : [g^k | let fs = fieldSize zero; g = pf $ P [(1,1)], k<-[1..fs-1]]
  where zero = pf $ P []
