import Lib.ConvPolynomial
import Lib.Polynomial (Poly(..), pscal)

-- 7.29

{-

λ> decryptNTRU ntru729 p729f p729e
+X^6-X^4+X

-}

decryptNTRU :: NTRUParams -> PolyCF -> PolyCF -> PolyC
decryptNTRU N{..} f e = m
  where a = liftCenter $ e * changeField (q,1) f
        m = liftCenter . sink (p,1) $ pF * a

        pF = liftCenter $ pinv f


data NTRUParams = N {n :: Integer, p :: Integer, q :: Integer}

ntru729 = N 7 3 37
p729f = PCF 7 (3,1) $ P [(-1,0),(1,1),(-1,3),(1,4),(1,5)]
p729Fp = PCF 7 (3,1) $ P [(1,0),(1,1),(-1,2),(1,4),(1,5),(1,6)]
p729e = PCF 7 (37,1) $ P [(2,0),(8,2),(-16,3),(-9,4),(-18,5),(-3,6)]

-- 7.30

{-

λ> encryptNTRU ntru730 h730 r730 m730
+14X^6+16X^5+20X^4+7X^3+19X^2+16X+23
λ> decryptNTRU ntru730 f730 it
-X^6-X^3-X^2+X+1

-}

encryptNTRU :: NTRUParams -> PolyCF -> PolyCF -> PolyCF -> PolyCF
encryptNTRU N{..} h r m = (pscal p `overCF` r) * h + m'
  where m' = changeField (q,1) m

ntru730 = N 7 3 29
h730 = PCF 7 (29,1) $ P [(3,0),(14,1),(-4,2),(13,3),(-6,4),(2,5),(7,6)]
m730 = PCF 7 (3,1) $ P [(1,0),(1,1),(-1,2),(-1,3),(-1,6)]
r730 = PCF 7 (29,1) $ P [(-1,0),(1,2),(-1,5),(1,6)]
f730 = PCF 7 (3,1) $ P [(-1,0),(1,1),(-1,2),(1,4),(1,6)]

-- 7.31

{-

log(q)/log(p), since

ciphertext is N numbers mod q, so N * log(q)
plaintext is N numbers mod p, so N * log(p)

-}

-- 7.32

{-

We know that

  e(x) ≡ ph(x) * r(x) + m(x) (mod q)

If p = q, then it follows that

  e(x) = m(x)

If p | q, then reduction modulo p will produce the plaintext.

-}

-- 7.33 ?

-- 7.34

{-

(a)
Now, that much is obvious:

  e1 ≡ ph * r + m1 (mod q)
  e2 ≡ ph * r + m2 (mod q)

  => e1 - e2 ≡ m1 - m2 (mod q)

m1 and m2 are ternary polynomials, so the coefficients of m1 - m2 are in the set
{ -2, -1, 0, 1, 2}. That means Eve can recover any coefficient precisely when the
coefficient is 2 or -2, meaning it's either 1 or -1 in m1 and the opposite in m2.
What's the expected value of that happening?

 (-1,1)       (1,-1)
1/3 * 1/3 + 1/3 * 1/3 = 2/9

So it checks out.

(b)

  e1 - e2 ≡ m1 - m2 ≡ -1 - 2x^2 - x^3 + 2x^4 - 2x^5

So we know coefficients in m1 for:

  -x^2 + x^4 - x^5

Let's substitute that into the formula for m1:

  m1 = a + bx - x^2 + cx^3 + x^4 - x^5 + dx^6 + ex^7

Also notice how we can't put any bounds on values for b, d and e, but we know that
a and c can't come out to 1. Now Eve only has to check 2 * 3 * 2 * 3 * 3 = 108 m's
instead of 3^8.

(c) If h(x) is invertible, easy. Denote h(x)^-1 as H:

  H(e1-e2) ≡ H(p*r1*h - p*r2*h) ≡ pHh(r1-r2) ≡ p(r1-r2) (mod q)

This case is basically the same as in (a), I imagine, being the difference of ternary
polynomials.
I don't know how to approach the problem without the invertibility assumption.

-}

-- 7.35

{-

First of all, f(x) is still invertible in both Rp and Rq.

(a) a ≡ f*e ≡ f*(h*r+m) ≡ f*(Fq*g*r+m) ≡ f*Fq*g*r + f*m ≡ g*r + f*m (mod q)
Center-lift it to R and then compute a (mod p):

  a ≡ g*r + f*m ≡ p*g0*r + (p*f0+1)m ≡ p*f0*m + m ≡ m (mod p)

(b) The proof of Prop. 7.48 seems reasonable, but I'm so lazy....

-}
