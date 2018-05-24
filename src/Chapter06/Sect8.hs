import Lib.EllipticCurve
import Lib (inv, binExpansion)
import Control.Applicative (liftA2)

-- 6.29 trivial, multiply out R*S and look at the div formula

-- 6.30

{-

skipped for now

-}

-- 6.31

{-

1 = em(P+Q,P+Q) = em(P,P+Q)em(Q,P+Q) = em(P,P)em(P,Q)em(Q,P)em(Q,Q) = em(P,Q)em(Q,P)
<=> em(Q,P) = em(P,Q)^-1

-}

-- 6.32

{-

(a) Th.6.36(a) effectively tells us that a rational function is uniquely determined
by its divisor up to a constant factor. So if we take functions with divisors in a form
we require, they're all practically the same.

(b) The divisor of a ratio of two rational function is a difference of their divisors.
With this:

  div(F) = div(f_P(Q+S)/f_P(S)) - div(f_Q(P-S)/f_Q(-S)) =
= div(f_P(Q+S)) - div(f_P(S)) - div(f_Q(P-S)) + div(f_Q(-S))
= m[P] - m[O] - m[P] + m[O] - m[Q] + m[O] + m[Q] - m[O] = 0
Now we use the fact (Th.6.36) that every function without zeros is constant. QED

-}

-- 6.33

{-

  em(P,Q) = em(a_P*P1+b_P*P2,a_Q*P1+b_Q*P2) =
  em(a_P*P1,a_Q*P1)em(a_P*P1,b_Q*P2)em(b_P*P2,a_Q*P1)em(b_P*P2,b_Q*P2) =
  em(a_P*P1,b_Q*P2)em(b_P*P2,a_Q*P1) = em(a_P*P1,b_Q*P2)/em(a_Q*P1,b_P*P2) =
  em(P1,P2)^{a_P*b_Q - a_Q*b_P}

-}

-- 6.34 is for Sect.6.9 I think, so I skipped it

-- 6.35

{-

λ> millerWeil p635a q635a s635a 5
671
λ> millerWeil p635b q635b s635b 7
749
λ> millerWeil p635c q635c s635c 7
105
λ> millerWeil p635d q635d s635c 7
394

-}

millerWeil :: ELPF -> ELPF -> ELPF -> Integer -> Integer
millerWeil p@ELPF{..} q s m = (((f_P (q+^s) * inv (f_P s) p') `mod` p') *
                                inv (f_Q (p -^ s) * inv (f_Q (negateP s)) p') p') `mod` p'
  where f_P = millerAlgo m p
        f_Q = millerAlgo m q
        p' = p_ curve_

millerG :: ELPF -> ELPF -> (ELPF -> Integer)
millerG p q O = error "points should be non-zero"
millerG p q s = case lambdaSlope p q of
                  Nothing -> (x - xP) `mod` p'
                  Just λ  -> ((y - yP - λ*(x - xP)) * inv (x + xP + xQ - λ^2) p') `mod` p'
  where (x,y) = q_ s; (xP,yP) = q_ p; (xQ,yQ) = q_ q
        p' = p_ . curve_ $ q


millerAlgo :: Integer -> ELPF -> (ELPF -> Integer)
millerAlgo m p = miller_aux p 1 mbin
  where miller_aux t f [] = let p' = p_ . curve_ $ p in (`mod` p') . f
        miller_aux t f (0:bin) = miller_aux (2*^t)     (f^2 * millerG t t) bin
        miller_aux t f (1:bin) = miller_aux (2*^t+^p) ((f^2 * millerG t t)*millerG (2*^t) p) bin

        _:_:mbin = reverse . binExpansion $ m

instance Num b => Num (a -> b) where
      negate      = fmap negate
      (+)         = liftA2 (+)
      (*)         = liftA2 (*)
      fromInteger = pure . fromInteger
      abs         = fmap abs
      signum      = fmap signum

ec635a = ElCurveF 0 23 1051
ec635b = ElCurveF (-35) (-9) 883
ec635c = ElCurveF 37 0 1009

p635a = ELPF ec635a (109, 203)
q635a = ELPF ec635a (240, 203)
s635a = ELPF ec635a (1,554)

p635b = ELPF ec635b (5, 66)
q635b = ELPF ec635b (103, 602)
s635b = ELPF ec635b (1,197)

p635c = ELPF ec635c (8, 703)
q635c = ELPF ec635c (49, 20)
s635c = ELPF ec635c (0, 0)

p635d = ELPF ec635c (417, 952)
q635d = ELPF ec635c (561, 153)

-- 6.36

{-

If E(F_q)[l] ≅ ℤ/lℤ, then this group should have a generator point, let's call it G.
We write P as kG and Q as jG. Rewriting the problem statement and using linearity:
  τ(P,Q) = τ(kG,jG) = τ(G,G)^kj = τ(G,G)^jk = τ(jG,kG) = τ(Q,P)

-}

-- 6.37

{-

τ(P,Q)/τ(Q,P) = (f_P(Q+S1)/f_P(S1)) / (f_Q(P+S2)/f_Q(S2))
Notice that the choice of S points doesn't really matter here, as in Weil pairing.
If we take S2=-S1 such that both points and functions with them plugged in are well-defined,
we obtain the Weil pairing as it's defined.

-}
