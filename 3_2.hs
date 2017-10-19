import Data.Numbers.Primes (primes, primeFactors)
import Data.List (nub)

data PubKey = PubKey {n_ :: Integer, e_ :: Integer} deriving (Show)
data PrvKey = PrvKey {p_ :: Integer, q_ :: Integer} deriving (Show)
-- ostensibly more sense to represent private key as d? but whatevs

-- 3.7

{-

(a) Ciphertext c === m^e === 892383^103 === 45293 (mod N = 2038667)
*Main> cipher 892383 103 2038667                  
45293
good

(b) Alice's PrvKey = PrvKey 1301 $ 2038667 `div` 1301

*Main> prvA
PrvKey {p_ = 1301, q_ = 1567}

*Main> decryptionExponent 103 prvA
810367

(c)
*Main> decipher 317730 pubA prvA
514407

-}

cipher :: Integer -> PubKey -> Integer
cipher m pub = m^e `mod` n
  where e = e_ pub
        n = n_ pub

decipher :: Integer -> PubKey -> PrvKey -> Integer
decipher c pub prv = let n = n_ pub in c^d `mod` n
  where d = let e = e_ pub in decryptionExponent e prv

-- decryptionExponent e (p,q) = d => ed === 1 (mod (p-1)(q-1))
decryptionExponent :: Integer -> PrvKey -> Integer
decryptionExponent e prv = inv e $ (p-1)*(q-1) `div` g
  where g = gcd (p-1) (q-1)
        p = p_ prv
        q = q_ prv

inv :: Integer -> Integer -> Integer -- multiplicative inverse using EEA
inv a p = if gcd a p == 1
          then norm . fst $ euc a p
          else error "can't compute inverse if gcd =/= 1"
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

-- 3.8

{-

*Main> primeFactors 12191
[73,167]

*Main> decipher 587 pubB prvB
4894

-}

-- 3.9

{-

*Main> factorByDifference npq39a                    
(677,521)
*Main> factorByDifference npq39b
(10007,7703)
*Main> factorByDifference npq39c
(17183,6367)
*Main> factorByDifference npq39d
(422183,407893)

-}

npq39a = (352717, 351520)
npq39b = (77083921, 77066212)
npq39c = (109404161, 109380612)
npq39d = (172205490419, 172204660344)

--factorByDifference (n, (p-1)(q-1)) = (p,q)
factorByDifference :: (Integer,Integer) -> (Integer, Integer)
factorByDifference (n,d) = (truncate p, truncate q)
  where pPlusq = n + 1 - d
        (p,q) = solveQuadrEq 1 (fromInteger ((-1) * pPlusq)) (fromInteger n)

-- сейчас бы решить квадратное уравнение через дискриминант
solveQuadrEq :: (Floating a) => a -> a -> a -> (a, a)
solveQuadrEq a b c = (r1,r2)
  where d  = b^2 - 4*a*c -- I assume D will always be > 0
        r1 = (-b + sqrt d) / 2*a -- since we have to have 2 roots (factors)
        r2 = (-b - sqrt d) / 2*a 

-- 3.10

{-

(a)
x := (p-1)(q-1)
e_1 * d_1 === 1 (mod x) <=> kx = e_1 * d_1 - 1
e_2 * d_2 === 1 (mod x) <=> lx = e_2 * e_2 - 1

It's obvious that x = gcd(kx,lx)/gcd(k,l)

To factor N, finding out (p-1)(q-1) =: x suffices, as we already know from 3.9.



-}
