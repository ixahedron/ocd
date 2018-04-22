module Lib ( (≡)
           , inv
           , euc
           , eGCD
           , mexp
           , ln
           , lnR
           , e
           , binom
           , crt
           , sbg
           , isBSmooth
           , isSquare
           , intSqrt
           , combs
           , solveEq
           , millerRabinTest
           , checkCarmichael
           , reduceToOdd
           , reduceToPrime
           , primePowers
           , binExpansion
           , jacobi
           ) where


import Data.Numbers.Primes (primes, isPrime, primeFactors)
import Data.List.Ordered (mergeAll)
import Data.List (nub, sort)
import Data.Maybe (fromMaybe)

infix 5 ≡
(≡) :: Integer -> Integer -> Integer -> Bool
a ≡ b = \p -> (a `mod` p) == (b `mod` p)

-- multiplicative inverse using EEA
inv :: (Integral n, Show n) => n -> n -> n
inv a p = if gcd a p == 1
          then norm . fst $ euc a p
          else error $ show p ++ " can't compute inverse if gcd =/= 1 " ++ show a
  where norm x = if x >= 0 then x else x+p

-- euc a b = (x,y) => ax + by = gcd a b
euc :: Integral n => n -> n -> (n,n)
euc a b | a `mod` b == 0 = (0,1)
        | otherwise = (y,x-y*(a `div` b))
  where (x,y) = euc b $ a `mod` b

-- same as euc but returning (x,y,gcd a b)
eGCD :: Integral n => n -> n -> (n,n,n)
eGCD a b | mod a b == 0 = (0,1,b)
         | otherwise = (y,x-y*(a `div` b),z)
        where
          (x,y,z) = eGCD b $ a `mod` b


-- modular (fast) exponentiation using binary method
-- mexp m a e = a^e (mod m)
mexp :: (Show n, Integral n) => n -> n -> n -> n
mexp 1 _ _ = 0
mexp m x y = raise x y
  where raise 0 _ = 0
        raise _ 0 = 1
        raise a 1 = a `mod` m
        raise a e | e < 0     = raise (inv a m) (-e)
                  | otherwise = let t = if e `mod` 2 == 1 then a `mod` m else 1
                    in t * (raise (a^2 `mod` m) (e `div` 2)) `mod` m

-- chinese remainder theorem
crt :: [Integer] -> [Integer] -> Integer
crt ns as = let prod = product ns
                ls = [let (_,x2) = euc x $ prod `div` x in x2 | x <- ns ]
            in (sum [ div (x*y*prod) z | (x,y,z) <- zip3 as ls ns ]) `mod` prod

-- natural logarithm
lnR :: Floating a => a -> a
lnR = logBase e

-- euler's constant
e :: Floating a => a
e = exp 1

-- ln for integer arguments, kinda legacy I guess?
ln :: Floating a => Integer -> a
ln = lnR . fromInteger

-- binomial coefficient (n k)
binom :: Integer -> Integer -> Integer
binom n k = product [n-k+1..n] `div` product [1..k]

-- is an integer a perfect square?
isSquare :: Integral n => n -> Bool
isSquare n = let s = intSqrt n in s*s == n

-- take an integer square root. Obviously only works
-- correctly if n = a², otherwise there's data loss.
intSqrt :: Integral n => n -> n
intSqrt = truncate . sqrt . fromIntegral

-- take a square root in a finite field
-- TODO fckin everything, this is problematic
sqrtFin :: Integer -> Integer -> Integer
sqrtFin p a | p ≡ 3 $ 4 = mexp p a' ((p+1) `div` 4)
            | otherwise = head $ [b | b<-[1..p-1], mexp p b 2 == a'] -- yeah yeah, unsafe, inefficient
  where a' = a `mod` p

-- all vectors of length n with elems taken from a given charset
-- combs [0,1] 3 = [[0,0],[0,1],[1,0],[1,1]] e.g.
combs :: [a] -> Integer -> [[a]]
combs cs n = mapM (const cs) [1..n]

-- b-smooth := has factors <=b only
isBSmooth :: Integer -> Integer -> Bool
isBSmooth b x = let xfactors = primeFactors x in all (<= b) xfactors

-- dumb bruteforce solver of equations modulo N
-- solveEq m n = [t] -> t² ≡ m (mod n)
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
-- reduceToOdd n = (k,p) => n = 2^k * p
reduceToOdd :: Integer -> (Integer, Integer)
reduceToOdd m = reduce 0 m
  where reduce k n | odd n = (k,n)
                   | otherwise = reduce (k+1) $ n `div` 2

-- reduces a number to just the largest of the factors
reduceToPrime :: Integer -> Integer
reduceToPrime n | isPrime n = n
                | otherwise = reduceToPrime $  n `div` (head . primeFactors $ n)

-- an infinite list of numbers with 1 unique factor
-- [2,3,4,5,7,8,9,11,...]
primePowers :: [Integer]
primePowers = mergeAll [[p^i | i <- [1..]] | p <- primes]

-- compute coefficients for the binary expansion of an integer
binExpansion :: Integer -> [Integer]
binExpansion 0 = [0]
binExpansion n = (n `mod` 2) : (binExpansion $ n `div` 2)

-- checks (using primeFactors, might be slow?) whether
-- a given number is Carmichael
checkCarmichael :: Integer -> Bool
checkCarmichael n | even n || isPrime n = False
                  | nub fcts /= fcts    = False
                  | otherwise           = all fermat fcts
  where fcts = primeFactors n
        fermat p = all (\a -> mexp p a n == a) [0..p-1]

-- Shank's babystep-giantstep algo for cracking DLP
-- NB: order calculation is inefficient as all hell
sbg :: Integer -> Integer -> Integer -> Integer -> Integer
sbg p order g h = i + j*n
  where n = 1 + flrt order
        --order = head . filter (\x -> mexp p g x == 1) $ [1..p]
        lg = sort [(g^k `mod` p, k) | k<-[0..n]]
        lh = sort [(h * inv (g^(n*k)) p `mod` p, k) | k<-[0..n]]
        (i,j) = fromMaybe (0,0) $ match lg lh

flrt :: Integer -> Integer  -- flrt x ≈ √x,  with  flrt x² ≤ x < flrt(x+1)²
flrt x = floor . sqrt . fromInteger $ x

match :: [(Integer, Integer)] -> [(Integer, Integer)] -> Maybe (Integer, Integer)
match [] _ = Nothing
match _ [] = Nothing
match xx@((x,i):xs) yy@((y,j):ys) | x == y = Just (i,j)
                                  | x > y = match xx ys
                                  | otherwise = match xs yy

-- Jacobi symbol
jacobi :: Integer -> Integer -> Integer
jacobi (-1) b | b `mod` 4 == 1 = 1
              | b `mod` 4 == 3 = -1
              | otherwise = error "even b"
jacobi   2  b | (b `mod` 8) `elem` [1,7] = 1
              | (b `mod` 8) `elem` [3,5] = -1
              | otherwise = error "even b"
jacobi   a  b | a `mod` b == 0 = 0
              | a `mod` b == b-1 = jacobi (-1) b
              | a `mod` b == 2   = jacobi 2 b
              | even a = let (k,p) = reduceToOdd a;
                             q = if even k then 1 else jacobi 2 b
                         in q * jacobi p b
              | odd b = case (a `mod` 4, b `mod` 4) of
                          (3,3) -> jacobi (-1) a * (jacobi (b `mod` a) a)
                          _ -> jacobi (b `mod` a) a
              | otherwise = error "even b"
