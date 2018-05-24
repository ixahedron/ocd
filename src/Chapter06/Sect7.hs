import Lib.EllipticCurveGeneralised
import Lib.Polynomial
import Control.Monad.State
import qualified Data.Map as M

-- 6.22

{-

  Case 1: x1 = x2, y1 + y2 + a1*x2 + a3 = 0 => P1+P2 = O:
if a1 = a3 = 0, we're back to our simplified case and this works.
Otherwise: the line connecting P1 and O is obviously of the form x = x1.
Substitute this into the equation for E and you get:
  Y^2 + a1x1Y + a3Y - x1^3 - a2x1^2 - a4x1 - a6 = 0

Treat this as a quadratic polynomial of Y, and name the two roots y0 and y0'.
Now obviously one of the two roots, say y0', is equal to y1, since we used
x1 to arrive at this point. What is the other one? Well, we know that (x1,y0)
must be equal to -P, since the square of both roots must be the same.

Now, apply some quadratic equation magic (hint: WolframAlpha), and we get:
  y0 = y2 = -y1 - a1x1 - a3

So in our algo's first case, P2 = -P1 and it makes sense that their sum is O.

  Case 2:

The line connecting P1 and P2 is given by y = kx + l for some k,l.

If we're sure that those k and l are actually λ and ν as they're defined, we
can proceed as in case 1: substitute λx+ν for y in the E equation, treat as a
(cubic) polynomial, find roots. This is inconvenient to typeset, so here's
the end result:

  x3 = λ^2 + a1λ - a2 - x1 - x2.
  y3 = λx3 + ν

Finally, we need to negate this as discussed in the beginning of the Chapter.

  P1 + P2 = P3 = (x3, -λx3 - ν - a1x3 - a3),

which is the desired result.

So the only thing left is to actually prove λ and ν as defined are what we need.

Case 2.1: P1 = P2
  The line we need is a tangent line to E at P1 = P2, so let's make use of the
  geometric meaning of a derivative and differentiate the equation, I guess?

  (Y^2 + a1XY + a3Y)' = (X^3 + a2X^2 + a4X + a6)'
  d/dx: a1Y = 3X^2+2*a2*X+a4
  d/dy: 2Y + a1X + a3 = 0

  So k = (3X^2 + 2*a2*X + a4 - a1Y)/(2Y + a1X + a3) = λ

  l = Y - λ*X = y - x*(3x^2 + 2*a2*x + a4 - a1y)/(2y + a1x + a3) =
    = (2y^2 + 2a1xy + a3y - 3*x^3 - 2*a2*x^2 - a4*x)/(2y + a1x + a3)
    = (2y^2+2*a1xy+2*a3y-a3y-2x^3-x^3-2*a2*x^2-2*a4*x+a4*x-2a6+2a6)/(2y + a1x + a3)
    (now we can vanish all the terms with coefficient 2, since they represent the curve's
    equation and so cancel each other out)
    = (- a3y - x^3 + a4*x + 2a6)/(2y + a1x + a3) = ν

  So λ and ν are as defined.

Case 2.2 P1 /= P2

  This is straightforward, make a the linear equation system from the two points you've
  got, solve it and get

  k = (y2-y1)/(x2-x1) = λ
  l = y1 - k*x1 = y1 - x1(y2-y1)/(x2-x1) = y1 - (x1y2-x1y1)/(x2-x1)
    = (y1*x2-y1*x1-x1*y2+x1*y1)/(x2-x1) = (y1*x2-x1*y2)/(x2-x1)
    = ν

  So λ and ν are as defined.

-}

-- 6.23

{-

(a) b2 = 1, b4 = 2T+1, b6 = 4T+5, b8 = 1-T^2
Δ = T^2-1 - 16T-8 - 27(4T+5)^2 + 9(2T+1)(4T+5) = T^2 + 1

λ> ecDiscriminant623
+T^2+1

(b) λ> map onCurve [p,q,r]
[True,True,True]

λ> p+^q
ELG (+X^2+X+1,+X^2+1)
λ> 2 *^ r
ELG (+X^2,+X)

(c) λ> listPoints ec623
[OG,ELG (+X^2,+X),ELG (+X^2,+X^2+X+1),ELG (+X^2+X+1,+X+1),ELG (+X^2+X+1,+X^2+1),ELG (+1,)]

(d) λ> findGenerator ec623
ELG (+X^2+X+1,+X+1)

-}

ecDiscriminant623 = ecDiscriminant ec623

poly623 = PolyF 2 poly623base
poly623base = P [(1,3),(1,1),(1,0)]

poly623a1 = poly623 $ P [(1,0)]; poly623a2 = poly623 $ P [(0,0)]; poly623a3 = poly623 $ P [(1,0)]
poly623a4 = poly623 $ P [(1,1)]; poly623a6 = poly623 $ P [(1,1),(1,0)]

ec623 = ECGen poly623a1 poly623a2 poly623a3 poly623a4 poly623a6

-- 6.24

{-

(a) Multiplication is trivial. Addition: per binomial theorem,
  (x+y)^p = Σ{j=0..p}(p j)x^j*y^(p-j)

x^p and y^p, the first and the last terms stay, their binomial coefficient being equal to 1.
Everything in the middle can be expanded as p!/j!(p-j!). p is a prime, so it can't be cancelled.
It follows that p divides the middle terms and so they are congruent to 0 in F_p^k.

(b) a^p-1 ≡ 1 in F_p, so naturally a^p ≡ a.

(c) Case 1, P = -Q:
  P+Q = O, τ(P+Q) = O^p = O

  τ(P) = (x^p,y^p), τ(Q) = (x^p, y'^p), where y' = -y-a1*x-a3. Per (a),
  y'^p = -y^p-a1^p*x^p-a3^p.
  E is defined over F_p, so all the as are in F_p. Use this to apply (b) and get:
  y'^p = -y^p-a1*x^p-a3. This gets us to the understanding that τ(P) = -τ(Q) and yes,
  O = τ(P) + τ(Q) = τ(P+Q) = O indeed.

Case 2, P = Q:
  τ(P) = τ(Q) = (x^p,y^p).
  τ(P) + τ(Q) = (τ(λ)^2 + a1*τ(λ) - a2 - 2*x^p, -(τ(λ)+a1)x3-τ(ν)-a3)

  I don't want to write out the entire expression for λ and ν in this glorious ascii file,
  but using (a) and (b) as in Case 1, we derive that in τ(λ) only xs and ys are to the power p.
  Same for τ(ν).

  P+Q = (λ^2 + a1λ − a2 − 2*x, −(λ + a1)x3 − ν − a3)
  τ(P+Q) = (τ(λ)^2 + a1*τ(λ) − a2 − 2*x^p, −(τ(λ)+a1)x3 − τ(ν) − a3)

  So again, τ(P)+τ(Q) = τ(P+Q).

Case 3, P/=Q is analogue to Case 2.

-}

-- 6.25

{-

(a) t_1 = 2 + 1 - #E(F_2) = 3-4 = -1
    t_2 = 2^2 + 1 - #E(F_4) = 2^2 + 1 - 2^2 - 1 + α^2 + β^2 = -3

      α^2 = ((-1+√-7)/2)^2 = (1 - 2*√-7 - 7)/4 = -6/4 = -1.5
      β^2 = ((-1-√-7)/2)^2 = (1 + 2*√-7 - 7)/4 = -6/4 = -1.5

  In general, t_k = α^k + β^k.


(b) 2*t_{k-2} = 2*α^{k-2} + 2*β^{k-2}
t_1*t_{k-1} = (α + β)(α^k + β^k) = α^k + αββ^{k-2} + βαα^{k-2} + β^k

αβ = (-1+√-7)(-1-√-7)/4 = (-1+√-7-√-7+7)/4 = 2

=> t_1*t_{k-1} = α^k + 2β^{k-2} + 2α^{k-2} + β^k

t_1*t_{k-1} - 2*t_{k-2} = α^k + β^k = t_k

(c) (d) λ> map curveSizeE0 [4,11,31,101]
[16,2116,2147574356,2535301200456455833701195805484]

-}


-- Assuming the Koblitz curve E0, take an integer p and compute #E0(F_2^p)
curveSizeE0 :: Integer -> Integer
curveSizeE0 k = let tt = evalState (t k) (M.empty :: M.Map Integer Integer) in 2^k + 1 - tt
  where t 1 = pure $ -1
        t 2 = pure $ -3
        t k = do
          tko <- lookup $ k-1
          tkt <- lookup $ k-2
          pure $ -1 * tko - 2*tkt

        lookup :: Integer -> State (M.Map Integer Integer) Integer
        lookup k = do
          m <- get
          case M.lookup k m of
            Just  t -> pure t
            Nothing -> do
              tt <- t k
              nm <- get
              put (M.insert k tt nm)
              pure tt


-- 6.26

{-

(a) As in 6.25, t_k = α^k + β^k. So:
p*t_{k-2} = p*α^{k-2} + p*β^{k-2}
t_1*t_{k-1} = (α + β)(α^k + β^k) = α^k + αββ^{k-2} + βαα^{k-2} + β^k

α and β have their abs equal to √p, so αβ = p. With this, we obtain the desired result.

(b) t_2 = t1*t1 - 2p
t_3 = t1*t2 - p*t1 = t1^3 - 3p*t1
t_4 = t1*t3 - p*t2 = t1^4 - 4p*t1^2 - 2p^2

-}

-- 6.27

{-

The part about l ≤ 2⌈log(n)⌉ + 1 is more or less clear, as n0 gets approx. halved
each iteration, but I'm in loss about anything else.

-}

-- 6.28

{-

λ> mapM_ (putStrLn . show . tauExpansion) [931, 32755, 82793729188]
-X^21-X^19-X^17+X^14+X^10+X^2-1
-X^31+X^28-X^22+X^19-X^17+X^15+X^8+X^6+X^4+X^2-1
-X^73-X^71+X^68+X^61+X^58+X^55-X^52+X^50-X^48+X^46+X^44-X^40+X^36-X^34+X^30-X^27-X^24+X^20+X^18+X^15-X^12-X^10+X^8+X^2

λ> map (deg . tauExpansion) [931, 32755, 82793729188]
[21,31,73]

λ> map (numberOfNonZeroTerms . tauExpansion) [931, 32755, 82793729188]
[7,11,24]

-}

tauExpansion :: Integer -> Poly
tauExpansion n = P . normalize $ tau_aux n 0 0
  where tau_aux 0  0  _ = []
        tau_aux n0 n1 i = let vi = if odd n0 then 2 - ((n0 - 2*n1) `mod` 4)
                                             else 0
                              hn0' = (n0 - vi) `div` 2
                          in (vi,i) : tau_aux (n1 - hn0') (-hn0') (i+1)

