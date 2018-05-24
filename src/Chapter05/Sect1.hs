import Data.List (permutations, nub, sort)

-- 5.1

{-

λ> sum . map (7^) $ [1..5]
19607

-}

-- 5.2

{-

(a) q^n
(b) product of q_i over i=1..n
(c) q^n²
(d)
(e) λ> 12*4*2*100*32
307200
(Bonus: no.)

-}

-- 5.3

{-

(a) ABC, ACB, BCA, BAC, CAB, CBA
(b) λ> map (concatMap show) . permutations $ [1..4]
["1234","2134","3214","2314","3124","1324","4321","3421",
"3241","4231","2431","2341","4123","1423","1243","4213",
"2413","2143","4132","1432","1342","4312","3412","3142"]
(c) 20!
(d) 7! = 5040
(e) Of how many symbols? Assuming 4 and using each letter once: 4*3=12

-}

-- 5.4

{-

(a) Since I'm too lazy to show it any other way:
λ> ... map concat . sort . permutations $ ["a","A","b","B"]
ABab, ABba, AaBb, AabB, AbBa, AbaB,
BAab, BAba, BaAb, BabA, BbAa, BbaA,
aABb, aAbB, aBAb, aBbA, abAB, abBA,
bABa, bAaB, bBAa, bBaA, baAB, baBA
λ> ... map concat . nub . sort . permutations $ ["a","a","b","b"]
aabb, abab, abba, baab, baba, bbaa

(b) Again, I don't understand the task. Using every symbol just once?
7! / (3!4!) then, for permuting 4 As and then 3 Bs among themselves.
So binom 7 3 = 35

(c) First, there's (9 4) ways to place As. Then, for every way
to place As, there's (5 3) ways to place Bs. Cs go to the remaining
spots.
λ> binom 9 4 * binom 5 3
1260

(d) First, let's look at how many different letter sets we could
end up with. It's guaranteed that we get at least two As and at
least 1 B. In fact, there are exactly three possibilities:
  AAAAB
  AAABB
  AABBB

For each of those we need to compute the number of combinations.

AAAAB: 5 ways to place the B, As go to the free spots => 5
AAABB: (5 2) ways for Bs => 10
AABBB: (5 2) ways for As => 10

So all in all, 25 words?

-}

binom :: Integer -> Integer -> Integer
binom n k = product [n-k+1..n] `div` product [1..k]

-- 5.5

{-

(a) |(student, prize)| = 100 X 5 = 500
(b) 100*99*98 ways to place winners, 5*4*3 ways to pick prizes
100*99*98*5*4*3 = 58212000
(c) same, but there's 5^3 ways to pick prizes
100*99*98*5*5*5 = 121275000
(d) (100 3) ways to choose winners, 5^3 to choose prizes
λ> 5^3 * binom 100 3
20212500

-}

-- 5.6

{-

λ> binomialth (M 5 [('z',1)]) (M 2 []) 3
8 + 60*z + 150*z^2 + 125*z^3
λ> binomialth (M 2 [('a',1)]) (M (-3) [('b',1)]) 4
81*b^4 - 216*a*b^3 + 216*a^2*b^2 - 96*a^3*b + 16*a^4
λ> binomialth (M (-2) [('x',0)])  (M 1 [('x',1)])  5
x^5 - 10*x^4 + 40*x^3 - 80*x^2 + 80*x - 32

-}

--          ^
--5 ('x',1) = 5x
--          ^
--1 ('y',2) = y²
-- yes, an extremely shady representation but seriously,
-- I most likely have to deal with just this one task
data Monom = M Integer [(Char, Integer)]

instance Show Monom where
  show (M a []) = show a
  show (M a ((_,0):xs)) = show $ M a xs
  show (M 0 _ ) = "0"
  show (M n ((x,e):xs)) = let p = if e == 1 then "" else "^" ++ show e;
                              sxs = if null xs then p else p ++ "*" ++ show (M 1 xs)
                              a rest = if n == 1 then rest else show n ++ "*" ++ rest
                          in a $ x : sxs

  showList xs = (++) $ intercalateSgn (map show xs)
    where intercalateSgn [] = []
          intercalateSgn [x] = x
          intercalateSgn (x:(('-':y):xs)) = x ++ " - " ++ intercalateSgn (y:xs)
          intercalateSgn (x:xs) = x ++ " + " ++ intercalateSgn xs

binomialth :: Monom -> Monom -> Integer -> [Monom]
binomialth (M a []) t2 n = binomialth (M a [('x',0)]) t2 n
binomialth t1 (M a []) n = binomialth t1 (M a [('x',0)]) n
binomialth (M a [(x,e1)]) (M b [(y,e2)]) n | (e1,e2) == (0,0) = [M ((a+b)^n) []]
                                           | x == y && e1 == e2 = [M ((a+b)^n) [(x,e1*n)]]
                                           | otherwise = bth_aux [0..n]
  where bth_aux [] = []
        bth_aux    [j] = [M (a^n) [(x,e1*n)]]
        bth_aux (0:js) =  M (b^n) [(y,e2*n)] : bth_aux js
        bth_aux (j:js)
          | x == y = M p [(x,e1*j+e2*(n-j))] : bth_aux js
          | otherwise = (M p . filter (\g -> snd g /= 0) $ [(x, e1*j), (y, e2*(n-j))]) : bth_aux js
            where q = binom n j; p = q*(a^j)*(b^(n-j));
binomialth _ _ _ = error "I only need binomial theorem on one-term monomials ¯\\_(ツ)_/¯"

-- 5.7

{-

(a)                 = n-j                 = n-(j+1)
   (n-1)! / (j-1)!(n-1-j+1)! + (n-1)! / j!(n-1-j)! =
   j(n-1)! / j!(n-j)! + (n-j)(n-1)! / j!(n-j)! =
   = (n-1)!(j+n-j) / j!(n-j)! = n(n-1)! / j!(n-j)! =
   = n! / j!(n-j)! = (n j)

(b) (x+y)^n = Σ_j=0..n (n j) x^j y^(n-j)
  = (x+y) Σ_j=0..n-1 (n-1 j) x^j y^(n-1-j)
  = Σ_j=0..n-1 (n-1 j) x^(j+1) y^(n-1-j) +
    Σ_j=0..n-1 (n-1 j) x^j y^(n-j)
  = Σ_j=1..n (n-1 j-1) x^j y^(n-j) +
    Σ_j=0..n-1 (n-1 j) x^j y^(n-j)
  = x^n + Σ_j=1..n-1 (n-1 j-1) x^j y^(n-j) +
    y^n + Σ_j=1..n-1 (n-1 j) x^j y^(n-j)

  So it mostly works

(c) It can indeed be decomposed, okay?
This is boring, maybe I'll come back to it

-}

-- 5.8

{-

(a) (p j) = p! / j!(p-j)!
j < p => p|p! => p|(p j)

(b) (a+b)^p = Σ_j=0..n (n j) a^j b^(n-j)
  = a^p + b^p + Σ_j=1..n-1 (n j) a^j b^(n-j)
And (a) tells us every term in the sum is divisible by p

(c) Base: a = 0
  0^p ≡ 0 (mod p) ✓

Assumption:
  a^p ≡ a (mod p)

Step: a -> a+1
         (b)         Ass.
  (a+1)^p ≡ a^p + 1^p ≡ a + 1^p ≡ a+1 (mod p) ✓

So it works for every a >= 0

(d) If gcd(p,a)=1, there exists an inverse to a (mod p).
  a^p * a^-1 = a * a^-1 => a^(p-1) = a^0 = 1 (mod p)

-}

-- 5.9

{-

(a) I lurked in my notes to Ex.1.5 and it seems that such
permutations are called derangements. The function is below.
λ> derangements 10
1334961
λ> derangements 26
148362637348470135821287825

(b) 1 or more letters fixed means it's not one of those
permutations where no letters are fixed.
λ> let n = 10 in product [1..n] - derangements n
2293839
λ> let n = 26 in product [1..n] - derangements n
254928823778135499762712175

(c) This is trickier. We can imagine that exactly one fixed letter
means we fix one number and then seek out those permutations where no
letters are fixed. There are n candidates for that initial fixation,
so all in all:
λ> let n = 10 in n * derangements (n-1)
1334960
λ> let n = 26 in n * derangements (n-1)
148362637348470135821287824

(d) Again, proceed as in (b):
λ> b10-c10
958879
λ> b26-c26
106566186429665363941424351

-}

derangements :: Integer -> Integer
derangements 0 = 1
derangements 1 = 0
derangements n = (n-1) * (derangements (n-1) + derangements (n-2))
