module Lib.EllipticCurveGeneralised (
                                      ECGen(..)
                                    , ELG(..)
                                    , onCurve
                                    , ecDiscriminant
                                    , curveSize
                                    , listPoints
                                    , findGenerator
                                    ) where

import Lib.AddGroup
import Lib.Polynomial

-- extremely simplified for now
type Field = PolyF

data ECGen = ECGen {a1 :: Field, a2 :: Field, a3 :: Field,
                    a4 :: Field, a6 :: Field} deriving (Eq,Show)

data ELG = OG | ELG {ec :: ECGen, q :: (Field,Field)} deriving (Eq)

instance Show ELG where
  show OG = "OG"
  show ELG{..} = "ELG " ++ show q

instance AddGroup ELG where
  zero = OG
  negateP OG = OG
  negateP (ELG ECGen{..} (x,y)) = ELG ECGen{..} (x,-y-a1*x-a3)
    where p' = p x

  (*^) _ OG = OG
  (*^) 0 p = OG
  (*^) 1 p = p
  (*^) k p = dadd p k

  OG +^ p2 = p2
  p1 +^ OG = p1
  p1 +^ p2 | (not . onCurve $ p1) || (not . onCurve $ p2) = error "points not on the curve"
           | p1 == negateP p2 = OG
           | negateP p1 == p2 = OG
  (ELG c1 (x1,y1)) +^ (ELG c2@ECGen{..} (x2,y2))
    | c1 /= c2 = error "not the same curve"
    | otherwise = let λ = if (x1,y1) == (x2,y2) then (3*x1^2+2*a2*x1+a4-a1*y1) * pinv (2*y1+a1*x1+a3)
                                                else (y2-y1) * pinv (x2-x1);
                      ν = if (x1,y1) == (x2,y2) then (-x1^3+a4*x1+2*a6-a3*y1) * pinv (2*y1+a1*x1+a3)
                                                else (y1*x2-y2*x1) * pinv (x2-x1);
                      x3 = λ^2+a1*λ-a2-x1-x2; y3 = -(λ+a1)*x3-ν-a3;
                  in ELG c1 (x3,y3)

-- Double-and-add algo for elliptic curves
dadd :: Integral n => ELG -> n -> ELG
dadd p n = dadd_aux n p OG
  where dadd_aux 0 _ r = r
        dadd_aux n q r | odd n = dadd_aux (n `div` 2) (q +^ q) (r +^ q)
                       | otherwise = dadd_aux (n `div` 2) (q +^ q) r

onCurve :: ELG -> Bool
onCurve OG = True
onCurve (ELG ECGen{..} (x,y)) = y^2 + a1*x*y + a3*y == x^3 + a2*x^2 + a4*x + a6

ecDiscriminant :: ECGen -> Field
ecDiscriminant ECGen{..} = -(b2^2)*b8 - 8*b4^3 - 27*b6^2 + 9*b2*b4*b6
  where b2 = a1^2 + 4*a2; b4 = 2*a4 + a1*a3; b6 = a3^2 + 4*a6
        b8 = a1^2*a6 + 4*a2*a6 - a1*a3*a4 + a2*a3^2 - a4^2

findGenerator :: ECGen -> ELG
findGenerator c = head . dropWhile (not . isGenerator) $ list
  where list = listPoints c
        isGenerator point = gen_aux point 1
          where gen_aux OG i = i == curveSize c
                gen_aux pt i = gen_aux (pt +^ point) (i+1)

listPoints :: ECGen -> [ELG]
listPoints c = OG : filter onCurve [ELG c (x,y) | x<-listPolyFs params, y<-listPolyFs params]
  where params = PolyF p quot; PolyF{..} = a1 c

curveSize :: ECGen -> Integer
curveSize = toInteger . length . listPoints
