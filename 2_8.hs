import Data.Numbers.Primes (primes, primeFactors)

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
crt (x:xs) = crt' xs (fst x,snd x)
  where crt' []         (r,m) = Just r
        crt' ((t,n):xs) (r,m) = if gcd m n == 1 then crt' xs (sol, m*n) else Nothing -- not coprime
          where sol = r + invy * m 
                invy = (inv m n * (norm t-r)) `mod` n

                norm x = if x >= 0 then x else x+n


inv :: Integer -> Integer -> Integer -- multiplicative inverse using EEA
inv a p = norm . fst $ euc a p
  where norm x = if x >= 0 then x else x+p

euc :: Integer -> Integer -> (Integer, Integer)
euc a b = case b of
            1 -> (0, 1)
            _ -> let (e, f) = euc b d
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

smsqrt :: Integer -> Integer -> [Maybe Integer]
smsqrt a m = [crt x | x<-combs ss]
  where ss = msqrt a m
        
        combs :: [(Integer, Integer)] -> [[(Integer, Integer)]]
        combs [] = []
        combs (s:ss) = (f s ss) ++ (f (-fst s, snd s) ss)

        combos ([]:ls) = combos ls
        combos ((h:t):ls) = map (h:) (combos ls) ++ combos (t:ls)
        
        f :: (Integer, Integer) -> [(Integer, Integer)] -> [[(Integer, Integer)]]
        f s ss = map (s:) $ combs ss

msqrt :: Integer -> Integer -> [(Integer, Integer)]
msqrt a m = msqrt' $ primeFactors m
  where msqrt'     [] = []
        msqrt' (x:xs) = (y,x) : msqrt' xs
          where y = if x `mod` 4 == 3 then b^((x+1) `div` 4) `mod` x else error "hooy"
                b = a `mod` x

-- 2.24

{-

(a) 

-}
