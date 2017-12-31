import Lib (isBSmooth, mexp, inv, euc)
import Data.Numbers.Primes (primeFactors)

-- 3.36

{-

(a)
λ> all id $ map (\i -> isBSmooth 5 $ mexp 19079 17 i) [3030, 6892, 18312]
True
(b) projobs, but the numbers are down there

(c)
λ> isBSmooth 5 $ (19 * (mexp p (inv 17 p) 12400)) `mod` p
True


3030 =  2*x1 + 6*x2 +   x3
6892 = 11*x1 + 2*x2
18312 = 4*x1 +   x2 + 3*x3


(0,0,0) mod 2
(8195,1299,7463) mod 9539

(17734,10838,17002) mod 19078

(d)
λ> let x = (12400 + 7*xx + yy) `mod` (p-1)
λ> x
13830
λ> mexp p 17 x
19

-}

crt :: [Integer] -> [Integer] -> Integer
crt ns as  = let prod = product ns
                 ls = [let (_,x2) = euc x $ prod `div` x in x2 | x <- ns ]
             in (sum [ div (x*y*prod) z | (x,y,z) <- zip3 as ls ns ]) `mod` prod
