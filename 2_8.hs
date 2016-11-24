import Data.Numbers.Primes (primes, primeFactors)
import Data.List (nub)

-- 2.18

{-

*Main> crt [(2,3),(3,7),(4,16)]
Just 164
*Main> crt [(3,7),(4,9)]
Just 31
*Main> crt [(137,423),(87,191)]
Just 27209
*Main> crt [(133,451),(237,697)]
Nothing

^ *Main> gcd 451 697
41
The modulae need to be coprime.

*Main> crt [(5,9),(6,10),(7,11)]
Just 986
*Main> crt [(37,43),(22,49),(18,71)]
Just 11733

-}


crt :: [(Integer, Integer)] -> Maybe Integer
crt [] = Nothing
crt ((f,s):xs) = crt' xs (f,s)
  where crt' []         (r,m) = Just r
        crt' ((t,n):xs) (r,m) = if gcd m n == 1 then crt' xs (sol, m*n) else Nothing -- not coprime
          where sol = r + invy * m 
                invy = (inv m n * (norm t-r)) `mod` n

                norm x = if x >= 0 then x else x+n


inv :: Integer -> Integer -> Integer -- multiplicative inverse using EEA
inv a p = norm . fst $ euc a p
  where norm x = if x >= 0 then x else x+p

-- euc a b = (x,y) => ax + by = (gcd a b)^2
euc :: Integer -> Integer -> (Integer, Integer)
euc a b = (g*x,g*y)
  where g = gcd a b
        (x,y) = euc' (a `div` g) (b `div` g)
        euc' a b = case b of
                 1 -> (0, 1)
                 _ -> let (e, f) = euc' b d
                    in (f, e - c*f)
          where c = a `div` b
                d = a `mod` b

-- 2.19

{-
*Main> crt [(2,3),(3,5),(2,7)]
Just 23
-}

-- 2.20

{-

That a+cm is a solution to x === a (mod m) is obvious by definition of remainder modulo.

  x = a + cm = a + (b-a) * m^-1 * m = a + b - a = b === b mod n

is also a true expression.

Every solution has the form a + cm + ymn:

  a + b `mod` m = a `mod` m + b `mod` m.
  
Use that to prove:
                                        = 0
  a + cm + ymn mod m === a + cm mod m + ymn mod m === a + cm mod m = a
  a + cm + ymn mod n === a + cm mod n + ymn mod n === a + cm mod n = b

QED
-}

-- 2.21

{-

(a) Expand the divisibility form:

a|c <=> c = ax
           gcd a b = 1
b|c => b|ax    =>     c = aby <=> ab|c QED

(b) 

-}

-- 2.23

{-

*Main> smsqrt 340 437
[Just 215,Just 291,Just 146,Just 222]
*Main> smsqrt 253 3143
[Just 1387,Just 2654,Just 489,Just 1756]
*Main> smsqrt 2833 4189
[Just 1712,Just 3187,Just 1002,Just 2477]
*Main> smsqrt 813 868
[Just 351,Just 393,Just 41,Just 83,Just 351,Just 393,Just 41,Just 83] - not all 8 distinct, ?

-}


smsqrt :: Integer -> Integer -> [Maybe Integer]
smsqrt a m = [crt x | x<-combs ss]
  where ss = msqrt a m
        
        combs :: [(Integer, Integer)] -> [[(Integer, Integer)]]
        combs [] = [[]]
        combs (s:ss) = let negfst (x,y) = (-x,y) in map (s:) (combs ss) ++ map ((negfst s):) (combs ss)

msqrt :: Integer -> Integer -> [(Integer, Integer)]
msqrt a m = nub . msqrt' . primeFactors $ m
  where msqrt'     [] = []
        msqrt' (x:xs) = (y,x) : msqrt' xs
          where y = if x `mod` 4 == 3 then b^((x+1) `div` 4) `mod` x else naiveroot
                b = a `mod` x

                naiveroot = head $ [c | c <- [1..x], c^2 `mod` x == b]

-- 2.24

{-

(a)

Note b^2 === a mod p <=> b^2 = a + qp.
If a = 0, b^2 = qp, which means b and q are both multiples of p, since p is prime. In this case, k never appears at all
and we can choose k arbitrarily.

In the following, let's assume without loss of generality that a /= 0.


                                                  b^2 = a + qp
  (b + kp)^2 = b^2 + 2bkp + k^2p^2 === b^2 + 2bkp     ===      a + qp + 2bkp = a + p(q + 2bk) mod p^2

For (b + kp)^2 to be congruent to a, we need to choose q + 2bk === 0 mod p^2 or q + 2bk === p mod p^2a.
In either case, q + 2bk === 0 mod p => q + 2bk = np, n in Z.
  
Let's reform that last result a bit: we need to find k,n so that np - 2bk = q.

  Recall Bezout's identity: if a and b /= 0, there exist integers x,y so that
    ax + by = gcd(a,b).
  If b and p are relatively prime, then with some luck we can apply this here!
  We'll just need to multiply x and y with q in the end.

So we only need to show that b and p are relatively prime.

Let's assume that b and p are not relatively prime. That means b = rp, r in Z, since p is prime.

  b = rp => b^2 = r^2p^2 === a mod p => a = 0, since p^2 === 0 mod p. This constitutes a contradiction. QED

(b) a /= 0, so we don't get an easy way out.

b^2 = a + qp => (537)^2 = 476 + 1291 * 223. We need to find

  223 + 2 * 537 * k = n * 1291

Bezout's identity with l = -k for readability:

  n * 1291 + 2 * 537 * l = 223.

*Main> (\(n,k) -> (n*q `mod` p, k*q `mod` p - p)) $ euc p (2*b)
(199,-239)

So k = 239 and (537 + 239*1291) = 309086

Let's check:

*Main> (b+k*p)^2 `mod` p^2
476

Meow! <3

(c)

-}

