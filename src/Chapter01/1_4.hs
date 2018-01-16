import Data.List (nub)
-- 1.28

{-
N = p1p2..pr + 1

If N is not prime, then it's divisible by some other prime. Let's assume
without loss of generality that pk | N with pk in our prime set.

Then, since pk | N and pk | p1p2pk..pr, it's also true that
  pk | N-p1p2..pr = 1.

This is obviously false and so pk is a new prime. QED
-}

-- 1.29

{-
Since gcd(a,b) = 1, there exists a solution so that
  au + bv = 1 => auc + bvc = c (1)

                                (1)
  a | bc => exists some k: ak = bc => c = auc + akv = a(uc+kv).

So a | c. QED

-}

-- 1.30

ord :: Integer -> Integer -> Integer
ord _ 1 = 0
ord a n | n `mod` a == 0 = 1 + ord a (n `div` a)
        | otherwise = 0

{-
*Main> ord 2 2816
8
*Main> ord 7 2222574487
5
*Main> map (\x -> ord x 46375) [3,5,7,11]
[0,3,1,0]
-}

-- 1.32

flt :: Integer -> Integer -> Integer
flt a p = a^(p-2)

-- 1.33

{-

a) If b is 1, good. Otherwise, b /= 1 (10/10 reasoning, I know). Then
                               LTF
  b^q = a^((p-1)/q*q) = a^(p-1) = 1.

So b's order must at least divide q. q is a prime though, so
either b's order is 1 (and b^1 = 1 => b = 1) or q. QED

b) phi(q) / p-1


-}

-- 1.34

upf :: Integer -> [Integer]
upf 1 = []
upf a = upf' a primes
  where upf' 1 _ = []
        upf' a p@(x:xs)
          | a `mod` x == 0 = x : upf' (a `div` x) p
          | otherwise      = upf' a xs           

isPR :: Integer -> Integer -> Bool
isPR p a 
    | p `mod` a == 0 = False
    | otherwise      = foldr (\x acc -> acc && (a^(s `div` x) `mod` p /= 1)) True ups
  where ups = nub $ upf s
        s = phi p

proots :: Integer -> [Integer]
proots p = filter (isPR p) [2..p-1]

phi :: Integer -> Integer
phi n = fromIntegral . length $ filter (\x -> gcd n x == 1) [1..n]

primes = sieve [2..]
  where sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

{-
*Main> proots 11
[2,6,7,8]
*Main> phi 10
4
*Main> proots 229
[6,7,10,23,24,28,29,31,35,38,39,40,41,47,50,59,63,65,66,67,69,72,73,74,77,79,87,90,92,96,98,102,105,110,112,113,116,117,119,124,127,131,133,137,139,142,150,152,155,156,157,160,162,163,164,166,170,179,182,188,189,190,191,194,198,200,201,205,206,219,222,223]
*Main> phi 228
72
*Main> length $ proots 229
72

*Main> filter (\p -> isPR p 2) $ takeWhile (<100) primes
[3,5,11,13,19,29,37,53,59,61,67,83]
*Main> filter (\p -> isPR p 3) $ takeWhile (<100) primes
[2,5,7,17,19,29,31,43,53,79,89]
*Main> filter (\p -> isPR p 4) $ takeWhile (<100) primes
[2]

-}

-- 1.35

{-

First, we note that q is prime, so p-1 must only have two divisors: p-1 = 2q.

Second, order of g must divide p-1 since it's a unit mod p.
We know that g^q ==/= 1 => order of g doesn't divide q. Similarly it can't be 1:
g ==/= 1 mod p. It also can't be 2 since we know from modular arithmetics that
  a1 === b1 and a2 === b2 mod p => a1a2 === b1b2 mod p. Rewrite
  g^2 === 1 mod p as g * g === +/- 1 * +/- 1 mod p
So g must be congruent to either 1 or -1 mod p, which is not the case.
It follows that order of g divides 2q but not 2 and not q, so it must equal p-1.

The order of a primitive root mod p must be p-1 since otherwise the generating
would loop back to 1 before producing all the elements.
Hence, g is a primitive root mod p. QED
-}

-- 1.38

{-
*Main> map (\p -> 2^((p-1) `div` 2) `mod` p) $ tail $ takeWhile (<20) primes
[2,4,1,10,12,1,18]
-}

{-
It seems that the values are either 1 or p-1.
                                                     LFT
Let's suppose b === a^(p-1)/2 mod p => b^2 === a^p-1 === 1

That means b^2 - 1 === 0 === (b-1)(b+1) mod p
  => either a^(p-1)/2 === 1 or a^(p-1)/2 === -1 mod p

-1 === p-1 mod p, so that means the possible values are indeed 1 and p-1. QED

-}
