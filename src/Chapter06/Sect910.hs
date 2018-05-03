import Lib.EllipticCurve
import Lib.Field (Fieldish(..), Fp2Elem, Moddable(..), mexp, shankBG)
import Data.List (find)

-- 6.34 same as 6.38

-- 6.38

{-

φ(P1+P2) = (-x3, αy3)

λ = (3x^2 + 1)/2y
x3 = λ^2 - 2x
y3 = λ(x-x3)-y

φ(P1) + φ(P2) = (-x, αy) + (-x, αy) = (-x3, αy3)

λ' = (3(-x)^2 + 1)/2αy = (3x^2 + 1)/2αy

x3' = λ'^2 - 2(-x) = -λ^2 + 2x = -x3
y3' = λ'(-x-x3')-αy = λ'(-x+x3)-αy = α(λ'(1/α)(-x+x3)-y) =
    = α(-λ(-x+x3)-y) = α(λ(x-x3)-y) = αy3

-}

-- 6.39

{-

Something something Th.6.11?
  #E(F_p) = p + 1 - t_p, |t_p| <= 2√p =>
  #E(F_p) <= p + 1 + 2√p = (p + 1)^2 < l^2

So #E(F_p) < l^2. Not sure how that helps yet.
Maybe so: if E has a point of order l, then l divides the #E(F_p).
So now if E(F_p)[l] is larger than ℤ/lℤ, then it is equal to ℤ/lℤ x ℤ/lℤ,
which would mean the number of elements is l^2, which is a contradiction.
This isn't very convincing but it's the best I can come up with.

-}

-- 6.40

{-

Easy: compare ê(aP,bP) with ê(P,cP).

  ê(aP,bP) = e(aP,φ(bP)) = e(P,φ(P))^ab
  ê(P,cP) = e(P,φ(cP)) = e(P,φ(P))^c

If the two quantities are equal, it follows that abP = cP.

-}

-- 6.41

{-

Ummmm... Is this trivial or am I missing something?
φ(P) = (-x,αy), φ(φ(P)) = (-(-x), ααy) = (x,-y) = -P

-}

-- 6.42

{-

We use Prop.6.50 and Prop. 6.53 to conclude that P and φ(P) form a basis
for E(F_p)[l]. Take an arbitrary Q with order l and rewrite using basis:
                                       Ex.6.41
  φ(Q) = φ(aP + bφ(P)) = aφ(P) + bφ(φ(P)) = aφ(P) - bP

So what happens with the Weil pairing here?

  e(Q,φ(Q)) = e(aP + bφ(P), aφ(P) - bP)
            = e(aP,aφ(P)) * e(aP,-bP) * e(bφ(P),aφ(P)) * e(bφ(P),-bP)
            = e(P,φ(P))^{a^2} * e(φ(P),P)^{ -b^2}
            = e(P,φ(P))^{a^2 + b^2}

We know that φ is an l-distortion map _for P_, so either that Weil pairing is
a primitive l-root of unity, or l|(a^2 + b^2)), according to the definition.

(I'm not sure how to prove that l can't divide (a^2 + b^2) yet.
But it's probably trivial.)

l|(a^2+b^2) <=> a^2+b^2 = kl, k in ℤ <=> a^2+b^2 ≡ 0 (mod l)
            <=> a^2 ≡ -b^2 (mod l)

Now we employ Jacobi symbols and remember that squares disappear because
-1*-1 = 1 and 1*1 = 1:

(a^2/l) = 1 = (-b^2/l) = (-1/l)(b^2/l) = (-1/l)

So (-1/l) = 1. We go back to the Theorem 3.62 about quadratic reciprocity
and notice that (-1/p) = 1 if p ≡ 1 (mod 4). But our l ≡ 3 (mod 4) by assumption!
So there's a contradiction and l does not divide (a^2+b^2). It must follow that
the Weil pairing we computed is a primitive l-root of unity.

-}

-- 6.43

{-

E: y^2 = x^3+1

(a) (βx)^3 + 1 = β^3x^3 + 1 = x^3 + 1 = y^2
So if P on E, φ(P) is also on E.

(b) If either P1, P2 or both are O, it's clear the addition law holds. Otherwise:

P1 = P2: λ = 3x^2/2y, x' = λ^2-2x, y' = λ(x-x')-y, P+P = (x',y')

  φ(P+P) = φ(λ^2-2x, λ(x-x')-y) = (βλ^2-2βx, λ(x-βx')-y)

  φ(P) = (βx,y). φ(P) + φ(P):
  λ' = 3φ(x)^2/2y = 3*β^2*x^2/2y = λβ^2
  x'' = λ'^2-2φ(x) = (λβ^2)^2-2βx = λ^2β^4 - 2βx = βλ^2 - 2βx = βx'
  y'' = λ'(φ(x)-x'')-φ(y) = λβ^2(βx-βx')-y = λ(β^3x-β^3x')-y = λ(x-x')-y = y'

P1 ≠ P2: λ = (y2-y1)/(x2-x1), x' = λ^2-x1-x2, y' = λ(x1-x')-y1, P1+P2 = (x',y')

  φ(P1+P2) = φ(λ^2-x1-x2, λ(x1-x')-y1) = (βλ^2-βx1-βx2, λ(x1-x')-y1)

  φ(P1) = (βx1,y1), φ(P2) = (βx2,y2). φ(P1) + φ(P2):
  λ' = (φ(y2)-φ(y1))/(φ(x2)-φ(x1)) = (y2-y1)/(βx2-βx1) = λ/β
  x'' = λ'^2-φ(x1)-φ(x2) = (λ/β)^2-βx1-βx2 = λ^2/β^2 -βx1-βx2 = β(λ^2/β^3-x1-x2) = βx'
  y'' = λ'(φ(x1)-x'')-φ(y1) = λ/β(βx1-βx')-y1 = λ(x1-x')-y1 = y'

QED

-}

-- 6.44

{-

(a) F*_p is a group of order p-1, and we have to have elements of order 6
here, since if (z^3) = -1, (z^3)^2 = 1. But p ≡ 2 (mod 3), so it's impossible.
On the other hand, F_{p^2} contains an element a = g^{(p^2 - 1)/6}, which satisfies
a^6 = 1, a^3 ≠ 1, so a^3 = -1.

(b) We've already proved in Ex.6.43b that φ(nP) = nφ(P) for all P ∈ E(K) and all n ≥ 1,
so we only check that e(P, φ(P)) is a primitive root of unity. To do that, we turn to
Proposition 6.50.
Probably the easiest to check will be (b): P ≠ O and φ(P) is not a multiple of P.

It's clear that P≠O and so φ(P)≠O. Suppose φ(P) is a multiple of P. The coordinates
of P=(x,y) are in F_p, but we already know that (βx,y) with x≠O can't lie in F_p,
because F_p does not contain a primitive cube root of unity, only F_{p^2} does.
Suppose then that P=(0,y). I won't prove that rigorously, but looks like such points
have order 3, which is less than 5 [citation needed] that is the lower bound on our l.
So φ(P) is not a multiple of P.

-}

-- 6.45

{-
λ> weil645 p645 p645 s645 173
(242,92)
-}

modWeil :: (Integral n, Fieldish a) => (ELP a -> ELPP) -> ELP a -> ELP a -> ELPP -> n -> Fp2Elem
modWeil φ p q = millerWeil (toLargerField p) (φ q)

toLargerField :: Fieldish a => ELP a -> ELPP
toLargerField (ELPF c (x,y)) = ELPF c (toP2 x, toP2 y)

weil645 = modWeil φ645

φ645 :: ELPF -> ELPP
φ645 (ELPF c (x,y)) = ELPF c (fromInteger $ -x, (0,y))

e645 = ElCurveF 1 0 691
p645 = ELPF e645 (301,14)

-- found manually
--λ> onCurve s645
--True
s645 = ELPF e645 ((133,348),(245,499))

teste = ElCurveF 1 0 547
testp = ELPF teste (67,481)
tests = ELPF teste ((256,110),(441,15))

-- 6.46

{-

This isn't really the MOV method, but it seems to be enough anyway?
Like, it probably only works on this exercise as the numbers are cherry-picked,
but I don't even need a T' or to compute #E, since there exists a φ map.

λ> mov φ645 p645 q646 s645 173
122
λ> 122*^p645 == q646
True

-}

mov :: (Integral n, Fieldish a) => (ELP a -> ELPP) -> ELP a -> ELP a -> ELPP -> n -> n
mov φ p q s l = shankBG p' l êPP êPQ
  where êPP = modWeil φ p p s l
        êPQ = modWeil φ p q s l
        p' = fromInteger . p_ . curve_ $ p


q646 = ELPF e645 (143,27)

-- 6.47

{-

(a) λ> showReadable $ pubK p647 278
"EP (726,1127)"
(b) λ> weil645 qB647 qC647 s647 431
(1444,1288)
(c) λ> mexp 1723 it 278
(68,428)
(d) λ> mexp 1723 (weil645 qA647 qC647 s647 431) 224
(68,428)
(e) λ> weil645 qA647 qB647 s647 431
(977,1163)
λ> find (\nC -> mexp 1723 it nC == (68,428)) [0..431]
Just 145

-}

type PrKey = Integer
type PbKey = ELPF

pubK :: ELPF -> PrKey -> PbKey
pubK p n = n*^p

e647 = ElCurveF 1 0 1723
p647 = ELPF e647 (668,995)
qA647 = pubK p647 278
qB647 = ELPF e647 (1275,1550)
qC647 = ELPF e647 (897,1323)

s647 = ELPF e647 ((0,3),(166,166))

-- 6.48

{-

Here we remember and use the fact that the Weil pairing is a natural map to F_q.
Let Eve compute ê(Q_A,P) = ê(P,P)^nA and ê(Q_B,Q_C) = ê(P,P)^{nBnC}.
Now she just needs to take those values as her g^a and g^b and solve the DHP in F_q.

-}

-- 6.49

{-

(a) It's clear that with the ability to solve ECDLP, Eve can recover s from P^Tom = sP.
If she can only solve the DLP in F_q, she can use the MOV-algo or ê(P,P^Tom) if there
exists a φ map.

(b) To decrypt a ciphertext addressed to Alice, Eve needs, as described in the textbook,
to know ê(P^Alice,P)^rs. We notice P^Tom = sP and C1 = rP. Eve solves the ECDHP problem
with P^Tom and C1 and gets K = rsP. Now it's a only a matter of computing
ê(P^Alice,K) = ê(P^Alice,rsP) = ê(P^Alice,P)^rs, which is enough to decrypt (C1,C2).

(c) Well, duh.
ê(P^Alice,P^Tom) gets her ê(P^Alice,P)^s, similarly ê(P^Alice,C1) = ê(P^Alice,P)^r.
Combine them, solving the DH problem in F*_q, and Eve's got herself the same quantity
ê(P^Alice,P)^rs, enabling her to decrypt everything.

-}
