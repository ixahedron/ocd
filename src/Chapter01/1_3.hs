-- 1.15

{-
a1 === a2 mod m <=> a1 = a2 + mk
b1 === b2 mod m <=> b1 = b2 + ml,    some k,l in Z

Addition:

(a1+b1) - (a2+b2) = a1-a2 + b1-b2 = mk + ml = m(k+l)

so a1+b1 === a2+b2 mod m

Subtraction is done analogously.


Multiplication:

a1b1 - a2b2 = (a2 + mk)(b2 + ml) - a2b2 = a2b2 + mkb2 + mla2 + mkml - a2b2 =
= mkb2 + mla2 + mkml = m(kb2 + la2 + mkl)

so a1b1 === a2b2 mod m



-}

-- 1.20

{-

a1 and a2 are units, so there exist their inverses, b1 := a1^-1 and b2:= a2^-1
with a1b1 === 1 mod m and a2b2 === 1 mod m

Let's show that b2b1 is the inverse of a1a2:

              === 1   1.13a
b2b1a1a2 = b2(b1a1)a2  ===   1*1 = 1 mod m.

So b2b1 is the inverse of ab, which obviously means said inverse exists,
which means ab is a unit. QED

-}

-- 1.21

{-
Per definition, phi(m) counts the number of relative primes less than m. If all the numbers less than m (except zero) are relatively prime to m, which means there are m - 1 (because gcd(0,m) = m), then m is prime. This works in the other direction by definition.
-}

-- 1.22

{-

(a) (m+1) / 2

(b) m = qb+1 for q = (m-1)/b
(-1)*m = = -qb - 1
so -qb = (-(m-1)/b)*b === 1 mod m
Let's add m to get out of the negative range
m + (-(m-1)/b)*b === 1 mod m

so the inverse is -(m-1)/b + mb

-}

-- 1.23

{-
Perfect square mod 4 can only be 0 or 1.
Proof:
a*a === (a mod 4)(a mod 4) mod 4    (1.13a)
Possible values of a mod 4: 0,1,2,3
0 * 0 === 0 mod 4
1 * 1 === 1 mod 4
2 * 2 === 0 mod 4
3 * 3 === 1 mod 4

m is odd, so 2m === 2 mod 4, because 2 * m === 2 * (m mod 4) mod 4,
                             and m mod 4 cannot be 0 as m is odd.
And either a^2 === 0 mod 4 or a^2 === 1 mod 4, as shown above.

So we have 2 possibilities, using 1.13a:
2m + a^2 === 2 + 0 = 2 mod 4
or
2m + a^2 === 2 + 1 = 3 mod 4

none of which are perfect squares as per the table above. QED
-}

-- 1.25

sqm :: Integer -> Integer -> Integer -> Integer
sqm g p n = sqm' g 1 p
  where sqm' a b p
              | p > 0 && odd p = sqm' (a*a `mod` n) (b*a `mod` n) (p `div` 2)
              | p > 0 = sqm' (a*a `mod` n) b (p `div` 2)
              | otherwise = b

{-*Main> sqm 17 183 256
113
*Main> sqm 2 477 1000
272
*Main> sqm 11 507 1237
322
-}

