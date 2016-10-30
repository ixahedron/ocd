-- 2.3

{-

(a) Assume a /= b, since otherwise the congruence is obvious. Then

  b = a + k * (p-1), k in Z,

since g is a primitive root.
This means that

  g^a === g^b mod p, since
                                               g^p-1 === 1
  g^b = g^(a + k(p-1)) = g^a * g^k(p-1) = g^a * (g^(p-1))^k === g^a mod p

It follows immediately that

  g^a === g^b mod p <=> a === b mod p-1,

since, again, g is a primitive root. QED


(b) g^(log_g h1h2) = h1h2 = g^(log_g h1) * g^(log_g h2) = g^(log_g h1 + log_g h2)
QED

(c) For better readability, let z = log_g h in         (b)
g^(log_g h^n) = h^n = g^z * g^z * ... n times ... * g^z = g^(z+z+...+z) = g^(n*z)
QED

-}

-- 2.4

dlog :: Integer -> Integer -> Integer -> Maybe Integer
dlog p g a = dlog' 0
  where dlog' n
            | n > p-1 = Nothing
            | g^n `mod` p == a = Just n
            | otherwise = dlog' $ n+1

{-
*Main> dlog 23 2 13
Just 7
*Main> dlog 47 10 22
Just 11
*Main> dlog 941 627 608
Just 18
-}

-- 2.5

{-

=>:

a has a square root <=> exists x so that x^2 === a mod p

                              2.3c
log_g a = log_g x^2 = log_g 2x = 2 log_g x is even, QED

<=:

log_g a even <=> exists x in Fp* so that log_g a = 2 * log_g x = log_g x^2
a = g^log_g a = g^log_g x^2 = x^2, which means x is a square root of a modulo p.
QED

-}

-- 2.6

{-

Alice's exponent:
*Main> dlog 1373 2 974
Just 587

Bob's B:
*Main> 2^871 `mod` 1373
805

Their secret key:
*Main> 974^871 `mod` 1373
397

which is equal to
*Main> 805^587 `mod` 1373
397

-}

-- 2.7

{-

(a) Use the DHP solving algorithm with A and B for input, compare output g^ab with C.
Equality => yes, otherwise no.

(b) ?
I assume it is equally hard in general to solve this problem and DHP, but I have no
proof beyond intuition.

-}
