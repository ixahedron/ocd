import Data.Numbers.Primes (primes, primeFactors)
import Data.List (nub)

data PubKey = PubKey {n_ :: Integer, e_ :: Integer} deriving (Show)
data PrvKey = PrvKey {p_ :: Integer, q_ :: Integer} deriving (Show)
-- ostensibly more sense to represent private key as d? but whatevs

-- 3.7

{-

(a) Ciphertext c ≡ m^e ≡ 892383^103 ≡ 45293 (mod N = 2038667)
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

-- decryptionExponent e (p,q) = d => ed ≡ 1 (mod (p-1)(q-1))
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
This seems to have something to do with (multiplicative orders)?


  a^(de) ≡ a (mod N = pq) <=> a^(de-1) ≡ 1 mod N    (i)

We note that the above shows

  de-1 ≡ 0 (mod (p-1)(q-1))

Now, both e and d must be relatively prime to (p-1)(q-1),
so they're odd. That means:

  l := (d*e-1) = r*2^s, r odd, s>0.                     (ii)

The rest is actually pretty much an application
of Ex. 2.25, which I have, luckily, done ages ago.
First of all, note that Ex. 2.25(a) tells us that
if there are any solutions of x^2 ≡ a (mod N = pq),
then there are four of them.

So a = 1 has four square roots. Now we see two of them right away,
because they are trivial: 1 and -1. So we are only interested in
x and -x with x ≡ 1 (mod N), x ≡ 1 (mod p) and x ≡ -1 (mod q).
Furthermore, if x is a non-trivial solution mod p, we find p by
taking gcd(x-1,N). From there, the factorisation is easily obtained.

Looking at (i) and (ii), we see that for every g in [1..n-1],

  g^l ≡ 1 (mod N),

and so g^(l/2) is a square root of unity modulo N.
If g^(l/2) is a trivial solution (+/-1), we ~need to go deeper~.
So if l/2 is even, find a further square root of that by dividing
this by another two and trying again. If l/2 is odd, this choice
of g was useless and we choose another one.

It's useful to note that g needs to be relatively prime to N.
Then we can try to use a few small primes (so that it's unlikely that
gcd(a,N)>1) as g. If we don't find a solution, we try the next g.

That seems to work, but I don't know if that's rigorous enough.
Also I'm not sure that's how it should be done in this case,
as I only use one pair (e,d)?

-}

ned310b = ([(10988423,16784693), (25910155,11514115)], 38749709)
ned310c = ([(70583995,4911157), (173111957,7346999), (180311381,29597249)], 225022969)
ned310d = ([(1103927639,76923209), (1022313977,106791263), (387632407,7764043)], 1291233941)

{-

(b) *Main> factorByMagicBox (fst ned310b) (snd ned310b)
Just (7247,5347)

(c) *Main> factorByMagicBox (fst ned310c) (snd ned310c)
Just (10867,20707)

(d) *Main> factorByMagicBox (fst ned310d) (snd ned310d)
Just (97151,13291)

-}

factorByMagicBox :: [(Integer,Integer)] -> Integer -> Maybe (Integer,Integer)
factorByMagicBox        [] _ = Nothing
factorByMagicBox ((e,d):_) n = iterateFactorisation (e*d-1) (takeWhile (<n) primes)
  where iterateFactorisation _ [] = Nothing
        iterateFactorisation t (g:gs)
          | even t    = let tt = t `div` 2;
                            x = mexp n g tt;
                            p = gcd n $ x-1
                        in if x > 1 && p > 1
                           then Just (p, n `div` p)
                           else iterateFactorisation tt (g:gs)
          | otherwise = iterateFactorisation (e*d-1) gs

-- modular (fast) exponentiation using binary method
-- mexp m a e = a^e (mod m)
mexp :: Integer -> Integer -> Integer -> Integer
mexp 1 _ _ = 0
mexp m a e = raise a e
  where raise 0 _ = 0
        raise _ 0 = 1
        raise a 1 = a `mod` m
        raise a e = let t = if e `mod` 2 == 1 then a `mod` m else 1
                    in t * raise (a^2 `mod` m) (e `div` 2) `mod` m


-- 3.11

{-
                                        ≡ 1 mod p         F.l.t.
(a) x ≡ c_1 ≡ m * g_1^(s_1) ≡ m * (g^(p-1))^(r_1 * s_1) ≡ m (mod p)
The same reasoning goes for x ≡ c_2 (mod q), and so it must be that
x ≡ m (mod N = pq).

(b)

  g_1 ≡ 1 (mod p)
  g_2 ≡ 1 (mod q)

This means we can use it:

     x :=       y :=
  (g_1 - 1) * (g_2 - 1) ≡ 0 (mod p*q = N)

so x*y is a multiple of N! Since N has only two (large) divisors,
let's now use gcd:

  gcd(x,N) = p,

as per p|(g_1 - 1). q is then obtained symmetrically, and we're done.

-}
