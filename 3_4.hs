import Lib
import Data.Numbers.Primes (primeFactors, isPrime, primes)
import Data.List (nub)

-- 3.14
{-

(a),(b)
*Main> map checkCarmichael [561, 1729, 10585, 75361, 1024651]
[True,True,True,True,True]

-}

-- I'll use primeFactors, the numbers are rather small.
checkCarmichael :: Integer -> Bool
checkCarmichael n | even n || isPrime n = False
                  | nub fcts /= fcts    = False
                  | otherwise           = all fermat fcts
  where fcts = primeFactors n
        fermat p = all (\a -> mexp p a n == a) [0..p-1]


{-

(a) After checking a^n === a (mod p) for every divisor p of n
we combine the results using Chinese remainder theorem.
Recall Ex.2.20: Given the system of equations
x === a (mod m)
x === b (mod n)
with gcd(m,n)=1 and using
c === (b-a) * m^-1 (mod n),
every solution x has the form a + cm + ymn for some y in Z.

We can use this result for our case. The system has the form of

x === a (mod m)
x === a (mod n)
so
c === (a-a) * m^-1 === 0 (mod n),
and so every x is congruent to a modulo mn.

We don't care that our Carmichael number may have more than two
divisors: even composite m and/or n will work as long as gcd(m,n)=1.
We'll just have to expand it recursively.

Notice this immediately gives us the answer to (d), why Carmichael
numbers have to have only distinct prime divisors: otherwise
the CRT wouldn't work.

(c) 

(e) Korselt's criterion:
A composite number n is a Carmichael number iff n = p_1*p_2*...*p_n
is a product of distinct primes and for every j (p_j - 1) | (n-1)

*Main> korselt [13,37,61]   
True
*Main> korselt [307,613,919]
True

-}

korselt :: [Integer] -> Bool
korselt ps = (nub ps == ps) && all (\pj -> (n-1) `mod` (pj-1) == 0) ps
  where n = product ps

-- 3.15
{-

*Main> let millerRabin = millerRabinTest 10
*Main> map millerRabin ex315list
[Left (-1),Left 2,Right [2,3,5,7,11,13,17,19,23,29],
Right [2,3,5,7,11,13,17,19,23,29],Left 2,
Right [2,3,5,7,11,13,17,19,23,29],Left 2]

Let's check how good our test is

*Main> map isPrime ex315list
[False,False,True,True,False,True,False]

noice

-}

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

reduceToOdd :: Integer -> (Integer, Integer)
reduceToOdd n = reduce 0 n
  where reduce k n | odd n = (k,n)
                   | otherwise = reduce (k+1) $ n `div` 2


ex315list = [1105, 294409, 294439, 118901509, 118901521, 118901527, 118915387]

-- 3.16
{-

I have a strong suspicion that's exactly what I've done
in 3.10. I've only used one exponent and did the rest with
the Chinese theorem, which is related to Miller-Rabin test.

-}

-- 3.17
{-
(a)
*Main> piFunc 20
8
*Main> piFunc 30
10
*Main> piFunc 100
25

(b)
*Main> map piFunc [10^x | x<-[2..5]] 
[25,168,1229,9592]
*Main> map piRatio [10^x | x<-[2..5]] 
[1.151292546497023,1.1605028868689988,1.131950831715873,1.1043198105999443]

Sure, it's plausible.

-}

piRatio :: Integer -> Double
piRatio x = (fromInteger . piFunc $ x) / (fromInteger x / ln x)

piFunc :: Integer -> Integer
piFunc = toInteger . length . piPrimes

piPrimes :: Integer -> [Integer]
piPrimes x = takeWhile (<=x) primes

ln :: Integer -> Double
ln x = let e = exp 1 in logBase e $ fromInteger x

-- 3.18
{-

*Main> pi1 25
3
*Main> pi3 25
5
*Main> map pi1 [10^x | x<-[1..5]]                     
[1,11,80,609,4783]
*Main> map pi3 [10^x | x<-[1..5]]
[2,13,87,619,4808]

*Main> map pi31Ratio [10^x | x<-[2..5]]
[1.1818181818181819,1.0875,1.0164203612479474,1.005226845076312]

(c) based on (b), pi3(X) is larger and

 lim   pi3(X)/pi1(X) = 1
X->inf

-}

pi1 :: Integer -> Integer
pi1 = toInteger . length . filter (\p -> p `mod` 4 == 1) . piPrimes

pi3 :: Integer -> Integer
pi3 = toInteger . length . filter (\p -> p `mod` 4 == 3) . piPrimes

pi31Ratio :: Integer -> Double
pi31Ratio x = (fromInteger . pi3 $ x) / (fromInteger . pi1 $ x)
