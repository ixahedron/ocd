module Lib ( inv
           , euc
           , mexp
           , ln
           , lnR
           , e
           , isSquare
           , intSqrt
           , combs
           , solveEq
           , millerRabinTest
           , checkCarmichael 
           , reduceToOdd
           , reduceToPrime
           , primePowers
           ) where


import Data.Numbers.Primes (primes, isPrime, primeFactors)
import Data.List.Ordered (mergeAll)
import Data.List (nub)


-- multiplicative inverse using EEA
inv :: Integral n => n -> n -> n 
inv a p = if gcd a p == 1
          then norm . fst $ euc a p
          else error "can't compute inverse if gcd =/= 1"
  where norm x = if x >= 0 then x else x+p

-- euc a b = (x,y) => ax + by = (gcd a b)^2
euc :: Integral n => n -> n -> (n,n)
euc a b = (g*x,g*y)
  where g = gcd a b
        (x,y) = euc' (a `div` g) (b `div` g)
        euc' a b = case b of
                 1 -> (0, 1)
                 _ -> let (e, f) = euc' b d
                    in (f, e - c*f)
          where c = a `div` b
                d = a `mod` b


-- modular (fast) exponentiation using binary method
-- mexp m a e = a^e (mod m)
mexp :: Integral n => n -> n -> n -> n
mexp 1 _ _ = 0
mexp m a e = raise a e
  where raise 0 _ = 0
        raise _ 0 = 1
        raise a 1 = a `mod` m
        raise a e | e < 0     = raise (inv a m) (-e)
                  | otherwise = let t = if e `mod` 2 == 1 then a `mod` m else 1
                    in t * (raise (a^2 `mod` m) (e `div` 2)) `mod` m

-- natural logarithm
lnR :: Floating a => a -> a
lnR = logBase e

-- euler's constant
e :: Floating a => a
e = exp 1

-- ln for integer arguments, kinda legacy I guess?
ln :: Floating a => Integer -> a
ln = lnR . fromInteger

-- is an integer a perfect square?
isSquare :: Integral n => n -> Bool
isSquare n = let s = intSqrt n in s*s == n

-- take an integer square root. Obviously only works
-- correctly if n = a^2, otherwise there's data loss.
intSqrt :: Integral n => n -> n
intSqrt = truncate . sqrt . fromIntegral

-- all vectors of length n with elems taken from a given charset
-- combs [0,1] 3 = [[0,0],[0,1],[1,0],[1,1]] e.g.
combs :: [a] -> Integer -> [[a]]
combs cs n = mapM (const cs) [1..n]

-- dumb bruteforce solver of equations modulo N
-- solveEq m n = [t] -> t^2 === m (mod n)
solveEq :: Integer -> Integer -> [Integer]
solveEq m n = solve_aux [1..n]
  where solve_aux [] = []
        solve_aux (t:ts) | mexp n t 2 == m `mod` n = t : solve_aux ts
                         | otherwise = solve_aux ts

-- Left means composite with a witness, Right - probably prime
-- s is how many potential witnesses to check
millerRabinTest :: Int -> Integer -> Either Integer [Integer]
millerRabinTest s n | n > 2 && even n = Left (-2)
                    | any (\a -> gcd a n > 1) as = Left (-1) 
                    | otherwise       = mrt_aux 0 $ map (\a -> mexp n a q) as
  where as = take s primes
        (k,q) = reduceToOdd $ n-1

        mrt_aux _    [] = Right as
        mrt_aux 0 (a:xs) | (a `mod` n) `elem` [1,-1,n-1] = mrt_aux 0 xs
                         | otherwise = mrt_aux 1 ((mexp n a 2):xs)
        mrt_aux j (a:xs) | j == k = Left $ as !! (s - length (a:xs))
                         | (a `mod` n) `elem` [-1,n-1] = mrt_aux 0 xs
                         | otherwise = mrt_aux (j+1) ((mexp n a 2):xs)

-- reduces a number to the product of all its odd divisors
reduceToOdd :: Integer -> (Integer, Integer)
reduceToOdd n = reduce 0 n
  where reduce k n | odd n = (k,n)
                   | otherwise = reduce (k+1) $ n `div` 2

reduceToPrime :: Integer -> Integer
reduceToPrime n | isPrime n = n
                | otherwise = reduceToPrime $  n `div` (head . primeFactors $ n)

-- an infinite list of numbers with 1 unique factor
-- [2,3,4,5,7,8,9,11,...]
primePowers :: [Integer]
primePowers = mergeAll [[p^i | i <- [1..]] | p <- primes]

-- checks (using primeFactors, might be slow?) whether
-- a given number is Carmichael
checkCarmichael :: Integer -> Bool
checkCarmichael n | even n || isPrime n = False
                  | nub fcts /= fcts    = False
                  | otherwise           = all fermat fcts
  where fcts = primeFactors n
        fermat p = all (\a -> mexp p a n == a) [0..p-1]
