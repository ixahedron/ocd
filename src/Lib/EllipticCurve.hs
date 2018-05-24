module Lib.EllipticCurve (ElCurveF(..)
                        , ELP(..)
                        , ELPF
                        , ELPP
                        , AdditiveGroup(..)
                        , lambdaSlope
                        , onCurve
                        , millerWeil
                        , millerAlgo
                        , millerG
                        , showReadable
                        , curveSize
                        , listPoints
                        , listPointsReadable
                        , listPointsFp2
                        , listPointsReadableFp2
                        ) where

import Lib.Field
import Lib (binExpansion, sqrtFins)
import Control.Applicative (liftA2)

data ElCurveF = ElCurveF {a_ :: Integer, b_ :: Integer, p_ :: Integer}
  deriving (Show, Eq)

type ELPF = ELP Integer
type ELPP = ELP Fp2Elem
data ELP a = O | ELPF {curve_ :: ElCurveF, q_ :: (a,a)}
  deriving (Show, Eq)

normalize :: Fieldish a => ELP a -> ELP a
normalize O = O
normalize (ELPF ec (x,y)) = let p = p_ ec in ELPF ec (x `pmod` p, y `pmod` p)

class AdditiveGroup p where
  -- identity
  zero :: p
  (+^) :: p -> p -> p
  (^+) :: p -> p -> p
  (^+) = (+^)
  -- inverse
  negateP :: p -> p
  -- subtraction
  (-^) :: p -> p -> p
  p -^ p' = p +^ negateP p'
  -- scalar multiplication
  (*^) :: (Integral a) => a -> p -> p
  (^*) :: (Integral a) => p -> a -> p
  p ^* k = k *^ p

instance Fieldish a => AdditiveGroup (ELP a) where
  zero = O
  negateP O = O
  negateP (ELPF ec (x,y)) = normalize $ ELPF ec (x,-y)

  (*^) _ O = O
  (*^) 0 _ = O
  (*^) 1 p = p
  (*^) k p = dadd p k

  O +^ p2 = p2
  p1 +^ O = p1
  p1 +^ p2 | (not . onCurve $ p1) || (not . onCurve $ p2) = error "points not on the curve"
  p1@(ELPF c1 (x1,y1)) +^ p2@(ELPF c2@ElCurveF{..} (x2,_))
    | c1 /= c2 = error "not the same curve"
    | otherwise = case lambdaSlope p1 p2 of
                    Nothing -> O
                    Just λ -> let x3 = (λ*λ-x1-x2);
                                  y3 = (λ*(x1-x3)-y1)
                              in normalize $ ELPF ElCurveF{..} (x3,y3)

lambdaSlope :: Fieldish a => ELP a -> ELP a -> Maybe a
lambdaSlope O _ = Nothing
lambdaSlope _ O = Nothing
lambdaSlope p1@(ELPF _ (x1,y1)) p2@(ELPF ElCurveF{..} (x2,y2))
  | p1 == negateP p2 = Nothing
  | otherwise = Just $ if (x1,y1) == (x2,y2) then ((3*x1*x1 + fromInteger a_) * pinv (2*y1) p_ ) `pmod` p_
                                      else ((y2-y1) * pinv (x2-x1) p_) `pmod` p_

-- Double-and-add algo for elliptic curves
dadd :: (Fieldish a, Integral n) => ELP a -> n -> ELP a
dadd p n0 = dadd_aux n0 p O
  where dadd_aux 0 _ r = r
        dadd_aux n q r | odd n = dadd_aux (n `div` 2) (q +^ q) (r +^ q)
                       | otherwise = dadd_aux (n `div` 2) (q +^ q) r

onCurve :: Fieldish a => ELP a -> Bool
onCurve O = True
onCurve (ELPF ElCurveF{..} (x,y)) = y*y ≡ (x*x*x + a'*x + b') $ p_
  where a' = fromInteger a_
        b' = fromInteger b_

millerWeil :: (Fieldish a, Integral n) => ELP a -> ELP a -> ELP a -> n -> a
millerWeil p q s m = (((f_P (q+^s) * pinv (f_P s) p') `pmod` p') *
                        pinv (f_Q (p -^ s) * pinv (f_Q (negateP s)) p') p') `pmod` p'
  where f_P = millerAlgo m p
        f_Q = millerAlgo m q
        p' = p_ . curve_ $ p

millerG :: Fieldish a => ELP a -> ELP a -> (ELP a -> a)
millerG _ _ O = error "points should be non-zero"
millerG p q s = case lambdaSlope p q of
                  Nothing -> (x - xP) `pmod` p'
                  Just λ  -> ((y - yP - λ*(x - xP)) * pinv (x + xP + xQ - λ*λ) p') `pmod` p'
  where (x,y) = q_ s; (xP,yP) = q_ p; (xQ,_) = q_ q
        p' = p_ . curve_ $ q


millerAlgo :: (Fieldish a, Integral n) => n -> ELP a -> (ELP a -> a)
millerAlgo m p = miller_aux p 1 mbin
  where miller_aux _ f      [] = let p' = p_ . curve_ $ p in (`pmod` p') . f
        miller_aux t f (0:bin) = miller_aux (t+^t)     (f*f*millerG t t) bin
        miller_aux t f (1:bin) = miller_aux (t+^t+^p) ((f*f*millerG t t)*millerG (t+^t) p) bin
        miller_aux _ _ (_: _ ) = error "binary expansion isn't binary"

        _:_:mbin = reverse . binExpansion $ m

instance Num b => Num (a -> b) where
      negate      = fmap negate
      (+)         = liftA2 (+)
      (*)         = liftA2 (*)
      fromInteger = pure . fromInteger
      abs         = fmap abs
      signum      = fmap signum

showReadable :: Show a => ELP a -> String
showReadable O        = "EP O"
showReadable ELPF{..} = "EP " ++ show q_

curveSize :: Integral n => ElCurveF -> n
curveSize = fromIntegral . length . listPoints

listPoints :: ElCurveF -> [ELPF]
listPoints e = O : map (ELPF e) (listPointsReadable e)

listPointsFp2 :: ElCurveF -> [ELPP]
listPointsFp2 e = O : map (ELPF e) (listPointsReadableFp2 e)

listPointsReadable :: ElCurveF -> [(Integer, Integer)]
listPointsReadable ElCurveF{..} = concatMap points [0..p_-1]
  where points x | jacobi (y2 x) p_ == -1 = []
                 | otherwise = [(x,y) | y<-sqrtFins p_ (y2 x)]

        y2 x = x*x*x + a_*x + b_

listPointsReadableFp2 :: ElCurveF -> [(Fp2Elem, Fp2Elem)]
listPointsReadableFp2 e@ElCurveF{..} = concatMap points [(i,j) | i<-[0..p_-1], j<-[0..p_-1]]
  where points x = [(x,y) | y<-[(i,j) | i<-[0..p_-1], j<-[0..p_-1]], onCurve $ ELPF e (x,y)]
