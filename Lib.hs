module Lib ( inv
           , euc
           , mexp
           ) where


import Data.Numbers.Primes (primes, isPrime, primeFactors)
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
