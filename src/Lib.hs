module Lib (
             ln
           , lnR
           , e
           , binom
           , crt
           , isBSmooth
           , isSquare
           , intSqrt
           , sqrtFins
           , sqrtFin
           , combs
           , solveEq
           , millerRabinTest
           , checkCarmichael
           , reduceToOdd
           , reduceToPrime
           , primePowers
           , binExpansion
           ) where


import Data.Numbers.Primes (primes, isPrime, primeFactors)
import Data.List.Ordered (mergeAll)
import Data.List (nub)
import Lib.Field


-- chinese remainder theorem
crt :: [Integer] -> [Integer] -> Integer
crt ns as = let prod = product ns
                ls = [let (_,x2) = euc x $ prod `div` x in x2 | x <- ns ]
            in sum [ div (x*y*prod) z | (x,y,z) <- zip3 as ls ns ] `mod` prod

-- natural logarithm
lnR :: Floating a => a -> a
lnR = logBase e

-- euler's constant
e :: Floating a => a
e = exp 1

-- ln for integer arguments, kinda legacy I guess?
ln :: (Floating a, Integral n) => n -> a
ln = lnR . fromIntegral

-- binomial coefficient (n k)
binom :: Integral n => n -> n -> n
binom n k = product [n-k+1..n] `div` product [1..k]

-- is an integer a perfect square?
isSquare :: Integral n => n -> Bool
isSquare n = let s = intSqrt n in s*s == n

-- take an integer square root. Obviously only works
-- correctly if n = a², otherwise there's data loss.
intSqrt :: Integral n => n -> n
intSqrt = truncate . sqrt . fromIntegral

-- all vectors of length n with elems taken from a given charset
-- combs [0,1] 2 = [[0,0],[0,1],[1,0],[1,1]] e.g.
combs :: Integral n => [a] -> n -> [[a]]
combs cs n = mapM (const cs) [1..n]

-- b-smooth := has factors <=b only
isBSmooth :: Integral n => n -> n -> Bool
isBSmooth b x = let xfactors = primeFactors x in all (<= b) xfactors

-- dumb bruteforce solver of equations modulo N
-- solveEq m n = [t] -> t² ≡ m (mod n)
solveEq :: Integer -> Integer -> [Integer]
solveEq m n = solve_aux [1..n]
  where solve_aux [] = []
        solve_aux (t:ts) | mexp n t 2 ≡ m $ n = t : solve_aux ts
                         | otherwise = solve_aux ts

-- take a square root in a finite field
-- TODO fckin everything, this is problematic
sqrtFins :: Integer -> Integer -> [Integer]
sqrtFins p a = [b | b<-[1..p-1], mexp p b 2 ≡ a $ p] -- yeah yeah, unsafe, inefficient

sqrtFin :: Integer -> Integer -> Integer
sqrtFin p a | p ≡ 3 $ 4 = mexp p a ((p+1) `div` 4)
            | otherwise = head $ sqrtFins p a

-- Left means composite with a witness, Right - probably prime
-- s is how many potential witnesses to check
millerRabinTest :: Integer -> Integer -> Either Integer [Integer]
millerRabinTest (fromIntegral -> s) n
  | n > 2 && even n = Left (-2)
  | any (\a -> gcd a n > 1) as = Left (-1)
  | otherwise = mrt_aux 0 $ map (\(fromIntegral -> a) -> mexp n a q) as
  where as = take s primes
        (k,q) = reduceToOdd $ n-1

        mrt_aux _    [] = Right as
        mrt_aux 0 (a:xs) | (a `mod` n) `elem` [1,-1,n-1] = mrt_aux 0 xs
                         | otherwise = mrt_aux 1 (mexp n a 2 : xs)
        mrt_aux j (a:xs) | j == k = Left $ as !! (s - length (a:xs))
                         | (a `mod` n) `elem` [-1,n-1] = mrt_aux 0 xs
                         | otherwise = mrt_aux (j+1) (mexp n a 2 : xs)

-- reduces a number to the product of all its odd divisors
-- reduceToOdd n = (k,p) => n = 2^k * p
reduceToOdd :: Integral n => n -> (n,n)
reduceToOdd = reduce 0
  where reduce k n | odd n = (k,n)
                   | otherwise = reduce (k+1) $ n `div` 2

-- reduces a number to just the largest of the factors
reduceToPrime :: Integral n => n -> n
reduceToPrime n | isPrime n = n
                | otherwise = reduceToPrime $ n `div` (head . primeFactors $ n)

-- an infinite list of numbers with 1 unique factor
-- [2,3,4,5,7,8,9,11,...]
primePowers :: Integral n => [n]
primePowers = mergeAll [[p^i | i <- [(1 :: Integer)..]] | p <- primes]

-- compute coefficients for the binary expansion of an integer
binExpansion :: Integral n => n -> [n]
binExpansion 0 = [0]
binExpansion n = (n `mod` 2) : binExpansion (n `div` 2)

-- checks (using primeFactors, might be slow?) whether
-- a given number is Carmichael
checkCarmichael :: Integer -> Bool
checkCarmichael n | even n || isPrime n = False
                  | nub fcts /= fcts    = False
                  | otherwise           = all fermat fcts
  where fcts = primeFactors n
        fermat p = all (\a -> mexp p a n == a) [0..p-1]
