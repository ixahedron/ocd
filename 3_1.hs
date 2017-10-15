import Data.Numbers.Primes (primes, primeFactors)
import Data.List (nub)
import Data.Ratio

-- 3.1

-- x^e ≡ c mod N
solve_congruence :: (Integer,Integer) -> Integer -> Integer -> Integer
solve_congruence (p,1) c e = let d = inv e (p-1) in c^d `mod` p
solve_congruence (p,q) c e = c^d `mod` (p*q)
  where d = inv e $ (p-1)*(q-1) `div` g
        g = gcd (p-1) (q-1)
        

{-

*Main> solve_congruence (97,1) 36 19         
36
*Main> solve_congruence (541,1) 428 137
213
*Main> solve_congruence (1159,1) 614 73 
598
*Main> solve_congruence (8023,1) 677 751
4858
*Main> solve_congruence (607,661) 328047 38993
36219

-}


inv :: Integer -> Integer -> Integer -- multiplicative inverse using EEA
inv a p = norm . fst $ euc a p
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

-- 3.2 skipped for now

-- 3.3

{-

If gcd p*q c > 1, that means that c is a multiple of either p or q, as they're both primes,
and so either p or q divides them (not both since c is taken mod pq,
so the only possible value of c is 0, and the solution is then also 0).
So let's take a look at all these cases.

i. gcd c p > 1 <=> p = ck for some k in Z.


-}


-- 3.4

phi_naive :: Integer -> Integer
phi_naive n = fromIntegral . length $ filter (\x -> gcd n x == 1) [1..n]

{-

(a)
*Main> phi_naive 6
2
*Main> phi_naive 9
6
*Main> phi_naive 15
8
*Main> phi_naive 17
16


(b) a prime number is, by definition, relatively prime to any other number except its multiples. So obviously φ(p)=p-1

(c) As per the hint, let's suppose without loss of generality that p∤a and look at the relevant multiples of a:

  a=k_1*a, k_2*a, ... k_φ(N)*a mod N, with k_i satisfying gcd(k_i,N)=1.

There are clearly max. φ(N) distinct numbers in this list, since φ(N) is exactly
the number of integers in [0,N] relatively prime to N.
They must be all different by the same logic as in Fermat's little theorem's proof (2 lazy, maybe expand later).
So now we have a list of φ(N) distinct numbers modulo N, and it's clear that multiplying
each of these numbers by a results in a permutation of the set, so:

  a=k_1*a, k_2*a, ... k_φ(N)*a ≡ k_1, k_2, ... k_φ(N) (mod N)

Factoring out all the a's, we get:

  a^φ(N) * Π^φ(N)_i=1 k_i ≡ Π^φ(N)_i=1 k_i (mod N)

Since none of the k_i's are divisible by N, their product won't be as well, so we can safely cancel it out.
We're left with:

  a^φ(N) ≡ 1 mod N

QED

-}

-- 3.5

{-

(a) φ(pq) = φ(p) * φ(q)

(b) φ(p^j) = p^j - p^(j-1)

There are p^j numbers between 0 and p^j - 1. We just need to remove from this list all the multiples of p.
How many are there? Every p-th number is a multiple of p, so, obviously, (p^j)/p = p^(j-1).
After subtracting from p^j - p^(j-1), only relatively prime to p^j numbers are left.

(c) (I hereby confess I've read the proof after despairing of working it out myself. Me 3stupid5thisshit :/)

(d) Let's start with the epiphany that we can rewrite (b) so that it kinda relates to the formula we're trying to prove:
  
  p^j - p^(j-1) = p^(j-1) * (p-1) = p^j * (1 - 1/p)

Yay! Now, let's recall 1.20, the Fundamental Theorem of Arithmetics, which states that any integer can be factored
as a unique product of prime powers:

  a = p_1^e_1 * p_2^e_2 * ... * p_r^e_r
  
Ummm... Okay, now we can probably use all this to rewrite φ(N) in the following matter:
  
      1.20           (c)                         (b)
  φ(N) = φ(Π p_i^e_i) = φ(p_1^e_1)*...*φ(p_r^e_r) = p_1^e_1 * (1 - 1/p_1) * ... * p_r^e_r * (1 - 1/p_r) = 
                     ooooh but the first product is exactly our N factorisation!
  = p_1^e_1*...*p_r^e_r * Π^r_i=1 (1 - 1/p_i) = N * Π^r_i=1 (1 - 1/p_i)

QED. Fuck me

-}

phi :: Integer -> Integer
phi n = numerator $ (n % 1) * product (map (\pi -> 1 - 1 % pi) n_factors)
  where n_factors = nub . primeFactors $ n

{-

(e)
*Main> phi 1728
576
*Main> phi 1575
720
*Main> phi 889056
254016

-}

-- 3.6

{-

(a) gcd(e,φ(N)) = 1  =>  e has an inverse modulo φ(N). Let's call it e^-1=d. Now we have:

  de ≡ 1 (mod φ(N)) <=> de = 1 + k*φ(N), k in Z.

  c^d ≡ (x^e)^d ≡ x^(ed) ≡ x (mod N)

This is still all very nashakaleno, so let's properly check that c^d is a solution by substituting x with it:
                                                                          Ex.3.4c
  (c^d)^e ≡ c^(de) ≡ c^(1+k*φ(N)) ≡ c * c^(k*φ(N)) ≡ c * (c^φ(N))^k ≡ c * 1^k ≡ c

So yeah, QED.

-}

solve_by_phi :: Integer -> Integer -> Integer -> Integer
solve_by_phi e c n = c^d `mod` n
  where d = inv e $ phi n

{-

(b)

*Main> solve_by_phi 577 60 1463
1390
*Main> solve_by_phi 959 1583 1625
147
*Main> solve_by_phi 133957 224689 2134440
1892929

-}
