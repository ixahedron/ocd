import Lib (binom)

-- 5.20

--The whole thing is boringly trivial. Just draw
--yourself some Venn diagrams if in doubt.

-- 5.21

{-

(a) 1/4
(b) 3/4
(c) 1/2
(d) λ> mapM_ (\(k,p) -> print $ (show k) ++ ": " ++ (show p)) (zip [0..10] $ map (kheads 10) [0..10])
"0: 9.765625e-4"
"1: 9.765625e-3"
"2: 4.39453125e-2"
"3: 0.1171875"
"4: 0.205078125"
"5: 0.24609375"
"6: 0.205078125"
"7: 0.1171875"
"8: 4.39453125e-2"
"9: 9.765625e-3"
"10: 9.765625e-4"
(e) sum $ map (kheads 10) [0,2..10]
0.5
(f) 1-(e) = 0.5

-}

--kheads n k: P that exactly k of n tosses are heads
kheads :: Integer -> Integer -> Double
kheads n k = let n_k = fromInteger $ binom n k
             in n_k * 0.5^k * 0.5^(n-k)

-- 5.22

{-

Yeah, I think?
λ> kheads 14 7
0.20947265625

So to just break even, I would need to win on average 
every fifth time, and 0.2095 is a bit better than 1/5.
So if I repeat the game 10000 times, I would most
probably win some money. How much? I would grab
λ> kheads 14 7 * 10000 * 4
8378.90625
from Alice, and give out
λ> (1 - kheads 14 7) * 10000 * 1
7905.2734375
in losses. Subtracting those numbers gives the net gain:
λ> 8378.90625 - it
473.63281251

-}

-- 5.23

{-

(a) Come on.
                        F,E disj.
(b) P(F|E) = P(F∩E) / P(E) = 0 / P(E) = 0

(c) If Fi, Fj and Fk are pairwise disjoint, (Fi∩Fj)∩Fk must
also be empty, since the union doesn't create any new elements
that could be elements of the third subset.
The rest follows from (a) and (b).

(d) Notice we can express E as
  E = ∪^n_i=1 (E∩Fi), because, as in (c), one has
  (E∩Fi)∩(E∩Fj)=0 for i ≠ j.

Then:
  P(E) = P(∪^n_i=1 (E∩Fi)) = ∑^n_i=1 P(E∩Fi)
= ∑^n_i=1 P(E|Fi)P(Fi)

(e) immediately from (d)

-}

-- 5.24

{-

(a) 1/2 * 7/10 + 1/2 * 4/12 = 0.51666
(b) P(U1|Pc) = P(Pc ∩ U1) / P(Pc) = (0.5 * 0.7)/(a) ≈ 0.677
(c) 1/2 * 7/10 * 6/9 + 1/2 * 4/12 * 3/11 = 0.2(78)

-}

-- 5.25

{-

(a) Let's say still 1/3 since I assume we haven't been told
by people before us which coins they drew.
(b) G - I drew gold, S - 5 people before me all drew silver

  P(S|G) = P(G|S)P(S) / P(G)

  P(G) = 1/3 per (a)
  P(G|S) = 10/25
  P(S) = kcoins 30 20 5 5
0.10879541914024672

  P(S|G) = 10/25 * 0.1088 / 1/3 ≈ 0.1306

-}

-- kcoins n m k i = Pr that you draw i of k silver coins
-- from an urn with m silver and n-m gold coins.
kcoins :: Integer -> Integer -> Integer -> Integer -> Double
kcoins n m k i = (m_i * nm_ki) / n_k
  where m_i = fromInteger $ binom m i
        nm_ki = fromInteger $ binom (n-m) (k-i)
        n_k = fromInteger $ binom n k

-- 5.26

{-

(a) P(B|J) = 0
P(J|B) = 0
P(J|C) = 1
(b) P(J|!A) = P(!J|!A) = 1/2
(c) P(A|J) = P(J|A)*P(A) / (P(J|A)P(A) + P(J|!A)P(!A))
P(J|A) = 1/2

P(A|J) = 1/2*1/3 / (1/2*1/3 + 1/2*2/3) = 1/3

(d) P(A|J) = P(J|A)*P(A) / (P(J|A)P(A) + P(J|!A)P(!A))
P(J|A) = 1

P(A|J) = 1*1/3 / (1*1/3 + 1/2*2/3) = 1/2
-}

-- 5.27

{-

(a) In order: 1/3, 2/3, switch
(b) ??? I think the answer must vary depending on the moment
Monty Hall forces Dan to stick with the first choice of curtains.
If Dan chooses a curtain and MH immediately locks that down,
Dan knows he doesn't get a car but can't do much about it.
If after the choice another goat curtain is opened, Dan should
stick with his first choice, since even if the car is behind
the yet unchosen curtain, MH will veto the switch, but otherwise
allow it in case of switching from car to goat.
Assuming the second scenario:

Cn - Dan chooses a car on the nth try
Gn - same for goats
F - MH forces Dan to stick with the first choice

P(C1) = 1/3, P(G1) = 2/3
P(F|C1,G2) = 0
P(F|G1,C2) = 1

With this:

P(C2|G1) = 0
P(G2|C1) = 2/3
P(C2|C1) = 1/3 - if Dan sticks with his first choice by himself

(c)
C1 - Dan chooses a car on the first try
C - Dan wins a car
G1, G - same for goats
F - sticks
S - switches

P(C1) = M/N
P(G1) = (N-M)/N

P(C|C1,F) = M/N
P(G|C1,F) = 0
P(C|G1,F) = 0
P(G|G1,F) = (N-M)/N

So the net probability P(C|F) = M/N.

Now let's say Dan chooses a car on his first try, with probability
M/N. If he's switching curtains now, he's trying to choose one
of M-1 cars from behind now N-K-1 curtains. So

  P(C|C1,S) = (M-1)/(N-K-1)
  P(G|C1,S) = 1-(M-1)/(N-K-1) = (N-K-M)/(N-K-1)

Now suppose Dan first chose a goat curtain.

  P(C|G1,S) = M/(N-K-1)
  P(G|G1,S) = (N-K-1-M)/(N-K-1)

If we sum over the events with switching, which we can easily
do since they're disjoint, we get:

  P(C|S) = M/N * (M-1)/(N-K-1) + (N-M)/N * M/(N-K-1) =
= (M(M-1) + M(N-M))/N(N-K-1) = M(N-1) / N(N-K-1)

So Dan should switch, if one has
  M(N-1) / N(N-K-1) > M/N <=>
   (N-1) / N(N-K-1) > 1/N <=>
   (N-1) /  (N-K-1) > 1

Which basically means that Dan should always switch if
Monty Hall revealed at least 1 goat (K>0).

-}

-- 5.28

{-

E - m doesn't have property A
F - No N times in a row

P(E|F) = ?

P(E|F) = P(F|E)P(E) / (P(F|E)P(E) + P(F|!E)P(!E))

P(E) = δ, P(!E) = 1-δ
P(F|E) = 1
P(F|!E) < (1-p)^N

P(E|F) = δ / (δ + (1-δ)*P(F|!E)) ≥ δ / (δ + (1-δ)(1-p)^N)

-}

-- 5.29

{-

(a) λ> pr529 0.9 0.75 25
0.9999999999999999
(b) λ> pr529 0.9 0.75 100
1.0

Seems suspiciously high, hmmm

(c) 0, isn't it? We are already 99% confident m doesn't
have the property A, even without testing it in any way,
because 99% of all numbers don't have property A.

(d) λ> confidence 0.99 0.5 0.9999
7

-}

pr529 :: Double -> Double -> Integer -> Double
pr529 δ p n = δ / (δ + (1-δ)*((1-p)^n))

-- confidence δ p c = n => n runs
confidence :: Double -> Double -> Double -> Integer
confidence δ p c = let denom = (c/δ)**(-1);  
                       conf = (denom - δ)/(1-δ)
                   in ceiling $ logBase (1-p) conf


-- 5.30

{-

A - x is composite
S - MRT fails N times

p = 0.75
δ = 1-1/lnx

Per Ex.5.28:

  P(!A|S) = 1-P(A|S) ≥ 1 - δ / (δ + (1-δ)*(1-p)^N)
  = 1 - (1-1/lnx) / (1-1/lnx + 1/lnx * 0.25^N)
  = ??? :/ 

-}      

-- 5.31

{-

What is this, a pediatric biology lesson? I assume kids
with larger shoe sizes are developing more rapidly, but I
don't really care.

-}

-- 5.32

{-

  ∑^n_k=0 fX(k) = ∑^n_k=0 (n k) * p^k * (1-p)^(n-k)

 bin.th.
    =     (p+1-p)^n = 1^n = 1

QED

-}

-- 5.33

{-

(a) D(∑^∞_n=0 x^n) = ∑^∞_n=0 D(x^n) = D(1/(1-x))
= D((1-x)^-1) = x * d/dx = x/(1-x)^2

The series converges for the same xs as before,
i.e.    |x| < 1

(b) ∑^∞_n=0 D(n*x^n) = n*∑^∞_n=0 D(x^n) = ∑^∞_n=0 n^2*x^n
                            *
  = D(x/(1-x)^2) = x * d/dx = x * (1+x) / (1-x)^3
  = x+x^2 / (1-x)^3
QED

*Using the quotient rule:
  (x/(1-x)^2)' = (x'*(1-x)^2 - x*((1-x)^2)') / ((1-x)^2)^2
= ((1-x)^2 - (-2(1-x))*x) / (1-x)^4 = (1-x)(1-x+2x) / (1-x)^4
= (1+x) / (1-x)^3

(c) (d) (e) proyobano


-}

-- 5.34

{-

(a) EX = ∑^(N-1)_i=0 xi * fX(xi) = 1/N ∑^(N-1)_i=0 i
= 1/N * 1/2 * (N-1) * N = (N-1)/2
(b) (N+1)/2
(c) There are 6 uniformly distributed numbers, so:
λ> 1/6 * sum [1, 3, 7, 11, 19, 23]
10.666666666666666
(d) Binomially distributed variable "consists" of n unitary
independent binomial variables (think about how flipping a
coin n times consists of n single flips), and obvs for this
unitary variable EX = p. So because of independency,
  EX = np.

-}
