module Lib.ConvPolynomial where

import Lib.Field (inv, (≡))
import Lib.Polynomial hiding (asToFieldF, fieldSize, overF, mult, pinv)

import Data.List (sortOn, groupBy, find)
import Data.Maybe (fromMaybe)
import Data.Function (on)

data PolyC = PC {n :: Integer, poly :: Poly}
data PolyCF = PCF {n :: Integer, pk :: (Integer,Integer), poly :: Poly}

convBase :: Integer -> (Integer,Integer) -> PolyCF
convBase n pk = PCF n pk $ P [(-1,0),(1,n)]

pcfEmpty :: Integer -> (Integer,Integer) -> PolyCF
pcfEmpty n pk = PCF n pk $ P []

liftNaive :: PolyCF -> PolyC
liftNaive PCF{..} = PC n poly

liftCenter :: PolyCF -> PolyC
liftCenter PCF{..} = PC n $ (f . reduceCoefs q) `over` poly
  where f = map (\(i,e) -> (if i <= q `div` 2 then i else i-q,e))
        q = let (p,k) = pk in p^k

sink :: (Integer,Integer) -> PolyC -> PolyCF
sink pk PC{..} = asToFieldF $ PCF n pk poly

overC :: ([(Integer,Integer)] -> [(Integer,Integer)]) -> PolyC -> PolyC
f `overC` PC{..} = PC n $ f `over` poly

overCinF :: ([(Integer,Integer)] -> [(Integer,Integer)]) -> PolyCF -> PolyCF
f `overCinF` PCF{..} = PCF n pk $ f `over` poly

overCF :: (Poly -> Poly) -> PolyCF -> PolyCF
f `overCF` (PCF n pk p) = PCF n pk $ f p

overF :: (PolyC -> PolyC) -> PolyCF -> PolyCF
f `overF` (PCF n pk p) = let PC _ pc = f $ PC n p in PCF n pk pc

instance Eq PolyC where
  (==) (nc -> PC n p1) (nc -> PC n' p2) = n == n' && p1 == p2

instance Eq PolyCF where
  (==) (ncf -> PCF n pk p1) (ncf -> PCF n' pk' p2) = n == n' && pk == pk' && p1 == p2

nc = overC normalize . asToRankN
cf = overCinF normalize . asToFieldF
ncf = overCinF normalize . overF asToRankN . asToFieldF

instance Show PolyC where
  show (PC n poly) = show poly

instance Show PolyCF where
  show (PCF n pk poly) = show poly

mult :: PolyCF -> PolyCF -> PolyCF
mult (PCF n pk _) (PCF n' pk' _) | n /= n' || pk /= pk' = error "polynomials must be of the same field/rank"
mult (PCF n pk p1) (PCF _ _ p2) = PCF n pk $ p1 * p2

convmult :: PolyC -> PolyC -> PolyC
convmult (PC n _) (PC n' _) | n /= n' = error "polynomials must be of the same rank"
convmult (PC n p1) (PC _ p2) = PC n . over (normalize . reduceExps n) $ p1 * p2

reduceExps :: Integer -> [(Integer,Integer)] -> [(Integer,Integer)]
reduceExps n = map (\(a,i) -> (a,i `mod` n))

asToRankN :: PolyC -> PolyC
asToRankN PC{..} = PC n . over (sortOn snd) $ reduceExps n `over` poly

asToFieldF :: PolyCF -> PolyCF
asToFieldF poly = let l = p^k;
                      (p,k) = pk poly
                  in reduceCoefs l `overCinF` poly

changeField :: (Integer,Integer) -> PolyCF -> PolyCF
changeField (p,k) PCF{..} = PCF n (p,k) poly

instance Num PolyC where
  (PC n a) + (PC n' b) | n /= n' = error "polynomials must be of the same rank"
                       | otherwise = asToRankN $ PC n (a + b)

  (*) = convmult
  abs (PC n a) = asToRankN $ PC n (abs a)
  signum (PC n a) = asToRankN $ PC n (signum a)

  negate (PC n a) = asToRankN $ PC n (negate a)

  fromInteger p = PC undefined (P [(p,0)])
  -- this is reliably breaking the logic, how do I fix this?

instance Num PolyCF where
  (PCF n pk a) + (PCF n' pk' b) | n /= n' || pk /= pk' = error "polynomials must be of the same rank"
                                | otherwise = asToFieldF $ PCF n pk (a + b)

  PCF{..} * b = overCinF normalize . sink pk $ convmult (liftNaive PCF{..}) (liftNaive b)
  abs PCF{..} = asToFieldF $ PCF n pk (abs poly)
  signum PCF{..} = asToFieldF $ PCF n pk (signum poly)

  negate PCF{..} = asToFieldF $ PCF n pk (negate poly)

  -- this is reliably breaking the logic, I know
  fromInteger p = PCF undefined undefined (P [(p,0)])


pdivCF :: PolyCF -> PolyCF -> (PolyCF, PolyCF)
pdivCF _ (overCinF normalize . asToFieldF -> PCF _ _ (P [])) = error "polynomial division by zero"
pdivCF (PCF n pk a) (PCF n' pk' b) | n /= n' || pk /= pk' = error "polynomials must be in the same field"
pdivCF (cf -> a@PCF{..}) (cf -> b)
    | degCF a < degCF b = (PCF n pk $ P [], a)
    | otherwise         = pdiv_aux (pcfEmpty n pk) a
  where pdiv_aux (cf -> k) (cf -> r)
          | degCF r < degCF b = (k,r)
          | r == pcfEmpty n pk = (k,r)
          | otherwise     = pdiv_aux (k + t) (r - t `mult` b)
          where t = PCF n pk $ P [(h, degCF r - degCF b)]
                h = (fst . highestCF $ r) * inv (fst . highestCF $ b) (p^e)
                (p,e) = pk

pdivC :: PolyC -> PolyC -> (PolyC, PolyC)
pdivC (PC n a) (PC n' b) | n /= n' = error "polynomials must be in the same field"
                         | otherwise = let (k,r) = pdiv a b
                                       in (asToRankN $ PC n k, asToRankN $ PC n r)

pmodC :: PolyC -> PolyC -> PolyC
pmodC a b = snd $ pdivC a b

pquotCF :: PolyCF -> PolyCF -> PolyCF
pquotCF a b = fst $ pdivCF a b

pmodCF :: PolyCF -> PolyCF -> PolyCF
pmodCF a b = snd $ pdivCF a b

pInvertible :: PolyCF -> Either String PolyCF
pInvertible PCF{..}
  | pgcd PCF{..} (convBase n pk) /= PCF n pk (P [(1,0)])
    = Left "gcd p (x^n-1) /= 1, so no inverse"
  | otherwise = Right $ pinv PCF{..}

pinv :: PolyCF -> PolyCF
pinv PCF{..} = let (x,y,gcd') = pEUC PCF{..} cbase;
                   cbase = convBase n pk
               in if gcd' == PCF n pk (P [(1,0)])
                  then x `pmodCF` cbase
                  else error "this polynomial doesn't have an inverse"

pgcd :: PolyCF -> PolyCF -> PolyCF
pgcd a b = (\(_,_,c) -> c) $ pEUC a b

-- euclidean algo on polynomials returning (x,y,gcd a b)
-- for ax + by = gcd a b
pEUC :: PolyCF -> PolyCF -> (PolyCF,PolyCF,PolyCF)
pEUC a@(PCF n pk pa) b@(PCF n' pk' pb) | n /= n' || pk /= pk' = error "polynomials must have the same field/rank"
pEUC (cf -> a@PCF{..}) (cf -> b) = let (x,y,z) = pEUC_aux a b;
                                       PCF _ _ z' = z
                                       (i,gcd') = contentFactoring z'
                                       (p,k) = pk
                                       norm = PCF n pk $ P [(inv i (p^k),0)]
                                   in (norm * x, norm * y, PCF n pk gcd')
  where pEUC_aux a b
          | pmodCF a b == PCF n pk (P []) = (pcfEmpty n pk,PCF n pk $ P [(1,0)],b)
          | otherwise = (y,x-y `mult` pquotCF a b,z)
              where
                (x,y,z) = pEUC_aux b $ a `pmodCF` b

degC :: PolyC -> Integer
degC (PC _ poly) = deg poly

degCF :: PolyCF -> Integer
degCF (PCF _ _ poly) = deg poly

highestCF :: PolyCF -> (Integer,Integer)
highestCF PCF{..} = highest poly

fieldSize :: PolyCF -> Integer
fieldSize PCF{..} = let (p,k) = pk in (p ^ k) ^ n
