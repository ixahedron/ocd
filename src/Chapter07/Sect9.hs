import Lib.Polynomial (Poly(..) )
import Lib.ConvPolynomial

-- 7.22

-- well, not by hand

{-

λ> a722a * a722b
-15X^2-11X-22
λ> b722a * b722b
-14X^4+9X^3-6X^2+2X+3
λ> c722a * c722b
+X^4+2X^3+3X^2+2X+1
λ> d722a * d722b
+4X^9+2X^8+3X^7+2X^6+4X^5+3X^4+2X^3+3X^2+2X+3

-}

a722a = PC 3 $ P [(-1,0),(4,1),(5,2)]
a722b = PC 3 $ P [(-1,0),(-3,1),(-2,2)]
b722a = PC 5 $ P [(2,0),(-1,1),(3,3),(-3,4)]
b722b = PC 5 $ P [(1,0),(-3,1),(-3,2),(-1,4)]
c722a = PC 6 $ P [(1,1),(1,2),(1,3)]
c722b = PC 6 $ P [(1,0),(1,1),(1,5)]
d722a = PC 10 $ P [(1,1),(1,2),(1,3),(1,4),(1,6),(1,7),(1,9)]
d722b = PC 10 $ P [(1,2),(1,3),(1,6),(1,8)]

-- 7.23

{-

λ> a723a * a723b
+6X^2+6X+4
λ> b723a * b723b
+3X^4+3X^3+X^2+X+1
λ> c723a * c723b
+2X^5+X^4+X^3+2X^2+2
λ> d723a * d723b
+X^6+X^4+X^2+X

-}

a723a = PCF 3 (7,1) $ P [(1,0),(1,1)]
a723b = PCF 3 (7,1) $ P [(-5,0),(4,1),(2,2)]
b723a = PCF 5 (2,2) $ P [(2,0),(2,1),(-2,2),(1,3),(-2,4)]
b723b = PCF 5 (2,2) $ P [(-1,0),(3,1),(-3,2),(-3,3),(-3,4)]
c723a = PCF 7 (3,1) $ P [(1,1),(1,3)]
c723b = PCF 7 (3,1) $ P [(1,1),(1,2),(1,4),(1,6)]
d723a = PCF 10 (2,1) $ P [(1,2),(1,5),(1,7),(1,8),(1,9)]
d723b = PCF 10 (2,1) $ P [(1,0),(1,1),(1,3),(1,4),(1,5),(1,7),(1,8),(1,9)]

-- 7.24

{-

(a) So if we express a(x) (mod (x-1)), we get:

  a(x) = (x-1) * b(x) + r(x),

and the remainder r here is either 0 or its degree is 0. So actually we
can find out r(x) by setting x = 1, because

  a(1) = (1-1) * b(x) + a(1), c in Z/qZ

And we're basically done.

(b) To be invertible, gcd of a(x) and (x^N-1) should be 1. But if a(1) ≡ 0,
obviously (x-1) | a(x). Furthermore, (x-1) | (x^N-1) and so gcd(a(x),x^N-1) /= 1

-}

-- 7.25

{-

a(1) = 1 + 1 + 1 ≡ 0 (mod 3), so it doesn't have an inverse.
b(1) = 1 + 1 - 1 ≡ 1 (mod 3). so it does:

λ> pinv b725
+2X^3+2X^2+2X+1

-}

b725 = PCF 5 (3,1) $ P [(1,0),(1,2),(-1,3)]

-- 7.26

{-

λ> pInvertible  a726
Right +7X^4+8X^3+3X^2+2X+3
λ> pInvertible  b726
Left "gcd p (x^n-1) /= 1, so no inverse"
λ> pInvertible  c726
Right +17X^6+4X^5+12X^4+18X^2+12X+10

-}

a726 = PCF 5 (11,1) $ P [(1,4),(8,1),(3,0)]
b726 = PCF 5 (13,1) $ P [(1,3),(2,1),(-3,0)]
c726 = PCF 7 (23,1) $ P [(20,6),(8,5),(4,4),(15,3),(19,2),(1,1),(8,0)]

-- 7.27

{-

(a)

  f(x) * G(x) = f(x) * F(x) * (2 - f(x) * F(x)) = 2f(x)F(x) - (f(x)F(x))^2

Subtract 1:

  2f(x)F(x) - (f(x)F(x))^2 - 1 = (f(x)F(x))^2 + 2f(x)F(x) - 1 = -(f(x)F(x) - 1)^2

Now remember f(x)F(x) ≡ 1 (mod p^i), so f(x)F(x) = k*p^i + 1:

  -(f(x)F(x) - 1)^2 = -(k*p^i)^2 = -k^2 * p^2i

So f(x)G(x)-1 ≡ 0 (mod p^2i), which means f(x)*G(x) ≡ 1 (mod p^2i)

(b) We need ceil(log_2 e) iterations, where each iteration consists of
2 convolution multiplications.

(c)
i. λ> pinvPE c727iP c727iI 4
+13X^4+10X^3+7X^2+5X
ii. λ> pinvPE c727iiP c727iiI 7
+12X^4+101X^3+34X^2+17X+1
iii. λ> pinvPE c727iiiP c727iiiI 5
+1710X^6+268X^5+710X^4+711X^3+840X^2+2430X+1142

-}

pinvPE :: PolyCF -> PolyCF -> Integer -> PolyCF
pinvPE f_ fF_ e_ = asToFieldF . changeField (p_,e_) $ pinvPE_aux f_ fF_ e_
  where (p_,_) = pk f_
        pinvPE_aux _ inverse 1 = inverse
        pinvPE_aux f@PCF{..} fF e | even e = pinvPE_aux f' doubleInverse (e `div` 2)
                                  | otherwise = pinvPE_aux f fF (e+1)
          where
            (p,k) = pk
            doubleInverse = let PCF _ _ poly' = fF' * (c2 - f'*fF')
                                fF' = changeField (p,k*2) fF
                                c2 = PCF n (p,k*2) $ P [(2,0)]
                            in PCF n (p,k*2) poly'
            f' = changeField (p,k*2) f

c727iP = PCF 5 (2,1) $ P [(1,2),(3,1),(7,0)]
c727iI = PCF 5 (2,1) $ P [(1,4),(1,2),(1,1)]

c727iiP = PCF 5 (2,1) $ P [(22,0),(11,1),(5,2),(7,3)]
c727iiI = PCF 5 (2,1) $ P [(1,0),(1,1),(1,3)]

c727iiiP = PCF 7 (5,1) $ P [(112,0),(34,1),(239,2),(234,3),(105,4),(180,5),(137,6)]
c727iiiI = PCF 7 (5,1) $ P [(2,0),(1,3),(3,5)]

-- 7.28

{-

(a)

E(∥b∥^2) = 2/3N is immediate from (-1)^2 = 1, 0^2 = 0, 1^2 = 1

E(∥a*b∥^2) = ∥a∥^2 * E(∥b∥^2): I guess ∥a∥^2 can be thought of as a constant,
which because of norm's properties and expected value's linearity can be factored out.

~~~A hand wave to you, my friends~~~


             norm p.           EV lin                 a fix
(d) E(∥a+b∥^2) = E(∥a∥^2 + ∥b∥^2) = E(∥a∥^2) + E(∥b∥^2) = ∥a∥^2 + E(∥b∥^2)

-}
