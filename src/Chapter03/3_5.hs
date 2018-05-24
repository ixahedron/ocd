import Lib (mexp)
import Data.Numbers.Primes (isPrime, primeFactors, primes)
import Data.Maybe (isJust)
import Data.List (transpose, tails)

-- 3.22

{-

λ> pollard 1739
Just (37,47)
λ> pollard 220459
Just (449,491)
λ> pollard 48356747
Just (6917,6991)

-}


-- pollard n = Just (p,q) means that n factors
-- into p and q with p-1 a product of small primes

-- any conscious way to choose that "specified bound" for j?
pollard :: Integer -> Maybe (Integer, Integer)
pollard n = let js = [3..100] in aux (nexp 2 2) js
  where aux _ [] = Nothing
        aux a (j:js) = let d = gcd n $ a-1
                       in if 1 < d && d < n
                          then Just (d, n `div` d)
                          else aux (nexp a j) js

        nexp = mexp n


-- 3.23

{-

(a) λ> map primeFactors [2^n-1 | n<-[2..10]]
[[3],[7],[3,5],[31],[3,3,7],[127],[3,5,17],[7,73],[3,11,31]]
So 3, 7, 31 and 127, unsurprisingly (for n = 2, 3, 5 and 7)

(b) λ> take 7 $ filter isPrime [2^n-1 | n<-[2..]]
[3,7,31,127,8191,131071,524287]

(c) if p is prime, then 2^p === 2 <=> 2^p - 1 === 1 (mod p).
Let's explore this:

Assume n = 2k. 2^n - 1 = 2^(2k) - 1 = (2^2)^k - 1

Recall the binomial something something factorization:

  a^n-b^n = (a-b)(a^(n-1) + a^(n-2)b + ... + ab^(n-2) + b^(n-1))

Use it (2^2)^k - 1:

  (2^2)^k - 1 = (2^2)^k - 1^k = (2^2 - 1) * ...

And so 3 | 2^(2k)-1. QED

(d) Similar argument for n = 3k:

  (2^3)^k - 1 = (2^3)^k - 1^k = (2^3 - 1) * ...

And 7 | 2^(3k)-1. QED

(e) We don't need the full factorization of n,
two (possibly non-prime) factors suffice. If n = pq, then:

  2^n - 1 = (2^p)^q - 1^q = (2^p-1) * ...

So 2^p - 1 | 2^n - 1 (and 2^q - 1 does too!). QED

(f) ugh, fine. 2^74207281 - 1, no larger primes known yet
(g) lol nope

-}

-- Sect. 3.6

-- 3.24

{-
λ> factorBySqDiff324 53357
[233,229]
λ> factorBySqDiff324 34571
[191,181]
λ> factorBySqDiff324 25777
[173,149]
λ> factorBySqDiff324 64213
[409,157]
-}

factorBySqDiff324 = factorBySqDiff 1 1

-- kill me
factorBySqDiff :: Integer -> Integer -> Integer -> [Integer]
factorBySqDiff k bI = factor_aux
  where factor_aux 1 = []
        factor_aux n
          | isSquare n = let l = factor_aux . intSqrt $ n in l ++ l
          | isPrime n = [n]
          | even n = 2 : factor_aux (n `div` 2)
          | otherwise = findB [bI..] -- living on the edge

          where findB (b:bs) = let a2 = k*n + b^2
                               in if isSquare a2
                                  then let a = intSqrt a2
                                           (p,q) = (gcd n $ a+b, gcd n $ a-b)
                                       in (++) (factor_aux p) $! factor_aux q
                                  else findB bs

isSquare :: Integral n => n -> Bool
isSquare n = let s = intSqrt n in s*s == n

-- not safeguarded, maybe TODO maybe not maybe fuck myself
intSqrt :: Integral n => n -> n
intSqrt = truncate . sqrt . fromIntegral

-- 3.25

{-

λ> factorBySqDiff 247 1 143041
[313,457]
λ> factorBySqDiff 3 36 1226987
[653,1879]
λ> factorBySqDiff 21 90 2510839
[1051,2389]

-}


-- 3.26

{-

λ> factorByLE d326a
Just (227,269)
λ> factorByLE d326b
Just (277,191)
λ> factorByLE d326c
Just (499,397)
λ> factorByLE d326d
Just (1637,1543)

-}

d326a = (61063,c326a)
d326b = (52907,c326b)
d326c = (198103,c326c)
d326d = (2525891,c326d)

c326a = [([1,3,1],1882),
         ([1,5,3],1898)]

c326b = [([5,1,1],399),
         ([6,1,0],763),
         ([6,5,0],773),
         ([1,0,3],976)]

c326c = [([3,3,5,0],1189),
         ([1,0,0,3],1605),
         ([5,3,3,0],2378),
         ([0,1,1,1],2815)]

c326d = [([1,0,1,2,1],1591),
         ([3,0,1,2,1],3182),
         ([1,2,1,2,1],4773),
         ([3,6,0,1,0],5275),
         ([4,2,3,1,1],5401)]

factorByLE :: (Integer, [([Integer], Integer)]) -> Maybe (Integer, Integer)
factorByLE (n,cs) = let solvs = combs $ map fst cs in tryOut solvs
  where ps = take (length . fst . head $ cs) primes
        tryOut    []  = Nothing
        tryOut (v:vs) = let res = filter (\x -> snd x /= 0) . zipWith (\vi (l,i) -> (l,vi*i)) v $ cs
                        in if isJust $ tryGCD res then res else tryOut vs

        tryGCD acs = let res = gcd n $ p - sqr acs;
                         p = product . map snd $ acs
                         sqr = product . zipWith (^) ps . map ((`div` 2) . sum) . transpose . map fst
                     in if res == 1 || res == n then Nothing else Just (res, n `div` res)


combs :: [[Integer]] -> [[Integer]]
combs cs = let n = length cs in mapM (const [0,1]) [1..n]
