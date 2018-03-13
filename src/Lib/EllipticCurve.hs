module Lib.EllipticCurve where

import Lib (inv, jacobi, (≡))

data ElCurveF = ElCurveF {a_ :: Integer, b_ :: Integer, p_ :: Integer}
  deriving (Show, Eq)

data ELPF = O | ELPF {curve_ :: ElCurveF, q_ :: (Integer, Integer)}
  deriving (Show, Eq)

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

instance AdditiveGroup ELPF where
  zero = O
  negateP O = O
  negateP (ELPF c@ElCurveF{..} (x,y)) = ELPF c (x,p_-y)

  (*^) _ O = O
  (*^) 0 p = O
  (*^) 1 p = p
  (*^) k p = dadd p k

  O +^ p2 = p2
  p1 +^ O = p1
  p1 +^ p2 | (not . onCurve $ p1) || (not . onCurve $ p2) = error "points not on the curve"
           | p1 == negateP p2 = O
           | negateP p1 == p2 = O
  (ELPF c1 (x1,y1)) +^ (ELPF c2@ElCurveF{..} (x2,y2))
    | c1 /= c2 = error "not the same curve"
    | otherwise = let λ = if (x1,y1) == (x2,y2) then (3*x1*x1+a_) * inv (2*y1) p_
                                                else (y2-y1) * inv (x2-x1) p_;
                      x3 = (λ^2-x1-x2) `mod` p_; y3 = (λ*(x1-x3)-y1) `mod` p_;
                  in ELPF ElCurveF{..} (x3,y3)

-- Double-and-add algo for elliptic curves
dadd :: Integral a => ELPF -> a -> ELPF
dadd p n = dadd_aux n p O
  where dadd_aux 0 _ r = r
        dadd_aux n q r | odd n = dadd_aux (n `div` 2) (q +^ q) (r +^ q)
                       | otherwise = dadd_aux (n `div` 2) (q +^ q) r

onCurve :: ELPF -> Bool
onCurve O = True
onCurve (ELPF ElCurveF{..} (x,y)) = y^2 ≡ (x^3 + a_*x + b_) $ p_

showReadable :: ELPF -> String
showReadable ELPF{..} = "EP " ++ show q_

curveSize :: Integral a => ElCurveF -> a
curveSize = fromIntegral . length . listPoints

listPoints :: ElCurveF -> [ELPF]
listPoints e = O:(map (ELPF e) $ listPointsReadable e)

listPointsReadable :: ElCurveF -> [(Integer,Integer)]
listPointsReadable e@ElCurveF{..} = concat . map points $ [0..p_-1]
  where points x | jacobi (x^3 + a_*x + b_) p_ == -1 = []
                 | otherwise = [(x,y) | y<-[0..p_-1], onCurve $ ELPF e (x,y)]
