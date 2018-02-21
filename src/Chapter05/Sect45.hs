import Lib (mexp, eGCD)
import Data.List (find)

-------- Sect. 5.4

-- 5.36

{-

(a)
λ> collisionPr 365 23
0.5072972343239857
λ> collisionPr 365 40
0.891231809817949
λ> 1 - (364/365)^200
0.4222980433009368

(b) 1 - Π_i=1..n-1 (1-i/N)

(c) Using
  e^-x ≥ 1-x
we get
   P(match) = 1 - Π_i=1..n-1 (1-i/N) ≥ 1 - e^(-1/N)*e^(-2/N)*...*e^(-(n-1)/N)
   = 1 - e^(∑_1..n-1(-i/N)) = 1 - e^(-∑_1..n-1(i)/N)
   = 1 - e^(-(n-1)n/2N)

-}

collisionPr :: Integer -> Integer -> Double
collisionPr d n = let days = fromInteger d
                  in 1 - product (map (\i -> 1 - (fromInteger i)/days) [1..n-1])


-- 5.37

{-

(a) 8/52 ≈ 15.4%
So I think that I'm justified in using the naïve formula (and not
1-P(no king of hearts)) since (unlike with birthdays) the deck isn't
separate from the cards you flip over. For example, there is no overly
large amount of flipped cards for which the probability would be larger
than one; you can flip max. 52 (n) cards. Furthermore, there can only
be one king of hearts in the sample since we don't replace cards, so
we're not overcounting anything. Does this matter? Thoughts?

(b) P(match) = 1 - P(all 8 cards are different)
= 1 - (1 - 8/52)*(1 - 7/52)*...
= 1 - Π(1 - (8-i)/52) = 0.4324262572686578

-}

-- 5.38

{-

(a) f(x) = e^-x - (1-x)
  f'(x) = -e^-x -0 + 1 = 1-e^-x

                   !
  f'(x) = 1 - e^-x = 0
  e^-x = 1 <=> -x = 0 <=> x = 0
  neg -> pos => minimum

  f(0) = 0 which means for all values x∈ℝ e^-x ≥ 1-x

(b) f(x) := (1-x)^a + 1/2ax^2 - e^(-ax)
  f(0) = 1 + 0 - 1 = 0
  f(1) = 0 + 1/2a - e^-a. a>1 => f(1) > 0

Which means if it ever dips below 0, there will be an inflection point.
So if f'(x)>0 on ]0;1[, we're done.

  f'(x) = -a*(1-x)^(a-1) + ax + a*e^(-ax) = a(x + e^(-ax) - (1-x)^(a-1))

Since a>1, factoring out a doesn't change the sign.

  g(x) = x + e^(-ax) - (1-x)^(a-1)

Now we need to use (a) somehow, since it... looks similar? ¯\_(ツ)_/¯

  e^-x ≥ 1-x | into (a-1)th power, which doesn't change the sign for a>1
  e^(-(a-1)x) ≥ (1-x)^(a-1)
  -(1-x)^(a-1) ≥ -e^(-(a-1)x)

This is almost what we need.

  g(x) = x + e^(-ax) - (1-x)^(a-1) ≥ x + e^(-ax) - e^(-(a-1)x)
  = x + e^(-(a-1)x)(e^-x - 1)

this expression must have its minimum at a = 1, since with respect to a
it rises monotonically for all x∈]0;1[.

h(x) = x + e^-x - 1 = e^-x - (1-x)

Now we get to use (a) again and confirm f'(x) > 0. QED

(c) a := m, x := n/N
P(≥1red) = 1 - (1-x)^a

This is just a straightforward application of (b):

  1 - (1-x)^a ≤ 1 - e^(-ax) + 1/2*ax^2 =>
  1 - (1-n/N)^m = P(≥1red) ≤ 1 - e^(-mn/N) + 1/2*m*(n/N)^2
  = 1 - e^(-mn/N) + mn^2/2N^2

Now if N is large and m and n ≈ √N, we can write this as approx.

  1 - e^(-mn/N) + 1/2√N, for which 1 - e^(-mn/N) is quite an accurate
  further approximation.

Q in QED is for questionable

-}

-- 5.39

{-

I don't want to write code for just this thing,
so using my elven eyes on the table:

g^234 = h * g^399
The solution (exponents): 234 * -399 ≡ -165 ≡ 645 (mod p-1 = 810)

-}

------- Sect. 5.5

-- 5.40

{-

λ> dlpOnRhoMatch 81799 11 41387 $ PExp 81756 9527 67782 28637
Just 64857

-}

data PollardExps = PExp {α :: Integer, β :: Integer, γ :: Integer, δ :: Integer}

dlpOnRhoMatch :: Integer -> Integer -> Integer -> PollardExps -> Maybe Integer
dlpOnRhoMatch p g h PExp{..} = find (\l -> mexp p g l == h) lhs
  where norm x = if x < 0 then x+p' else x
        a = norm $ α-γ; b = norm $ δ-β; p' = p-1
        (s,_,d) = eGCD b p'; w = ((a * s) `mod` p') `div` d
        lhs = [w + k*(p' `div` d) | k<-[0..d-1]]
  

-- 5.41

{-

λ> dlpPollardRho 7963 7 3018
Just 5238
λ> mexp 7963 7 5238
3018

-}

-- 5.42

{-

λ> dlpPollardRho 5011 2 2495
Just 3351
λ> dlpPollardRho 17959 17 14226
Just 14557
λ> dlpPollardRho 15239131 29 5953042
Just 2528453

-}

dlpPollardRho :: Integer -> Integer -> Integer -> Maybe Integer
dlpPollardRho p g h = let (y,γ,δ) = fRho (g,1,0)
                      in dlpOnRhoMatch p g h $ rhoxps g y $ PExp 1 0 γ δ
  where rhoxps x y exps | x == y = exps
        rhoxps x y PExp{..} = let (x',α',β') = fRho (x,α,β);
                                  (y',γ',δ') = fRho . fRho $ (y,γ,δ)
                              in rhoxps x' y' $ PExp α' β' γ' δ'
        
        fRho :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
        fRho (x,α,β) | p'/3 > x' && x' >= 0 = ((g*x) `mod` p, (α+1) `mod` (p-1), β)
                     | 2*p'/3 > x' && x' >= p'/3 =
                           ((x^2) `mod` p, (2*α) `mod` (p-1), (2*β) `mod` (p-1))
                     | p' > x' && x' >= 2*p'/3 = ((h*x) `mod` p, α, (β+1) `mod` (p-1))
          where x' = fromInteger $ x `mod` p; p' = fromInteger p


-- 5.43

{-  T h e  f u c k  is this shit

So after much muchhhh struggle I came up with this masterpiece:
https://ix.2038.io/photo5794355021033352510.jpg
I'm not even capable of caring if I got it right anymore. so sad.....

The result: I^2 = π/2 => I = √(π/2)

-}

-- 5.44

{-

(a) Might be related to the birthday or collision theorem, I think?
Like, if we're drawing (pseudo)random numbers and compute if their
difference divides/has a nontrivial factor in common with N, this
increases our chances of hits in the same way that asking if two
people have the same birthday instead of if two people have their
birtday on the same specific date increases the probability of an
affirmative answer. By those theorems, indeed k = O(√N).

(b) 
λ> mapM_ (putStrLn . show) $ map factorPollardRho543b [2201, 9409613, 1782886219]
Just (31,3,6.394568344792313e-2)
Just (541,33,1.0757913769276532e-2)
Just (7933,125,2.9603850270054325e-3)

(c)
λ> mapM_ (putStrLn . show) $ map factorPollardRho543c [2201, 9409613, 1782886219]
Nothing
Just (541,5,1.6299869347388687e-3)
Just (7933,67,1.5867663744749119e-3)

Seems like the computation is accelerated.

(d) The two lists will go on with gcd(|x'-y'|, n)=1 until precisely N
at which point my algo will return Nothing.

(e) with x := x0, y := y0 = x1 = x^4
x0,x1,x2,... = x^2, x^4,  x^8,...
y0,y1,y2,... = x^4, x^16, x^32,...

So we see that, first of all, sequences cycle very quickly, and also that
there's no path off the track, it will always be x^(2^i). Additionally, if you
either start or at some point bump into 0 or 1, you won't ever change the value
again. So this function behaves very poorly. I assume there are a few cases
when even x^2 might work, especially with well-picked initial values (like if
you pick one of the factors for the initial value, lol), but they must be rare.

(f) neehooya ne vyvezeno

-}

factorPollardUsing :: (Integer -> Integer) -> Integer -> Maybe (Integer, Integer, Double)
factorPollardUsing f n = rho_aux (f 0) (f . f $ 0) 1                            
  where rho_aux x y k | x  == y   = Nothing
                      | gk == n   = Nothing
                      | gk > 1    = Just (gk, k, k' / sqrt n')
                      | otherwise = rho_aux x' y' $ k+1
          where gk = gcd (abs $ x'-y') n
                x' = f x; y' = f . f $ y
                k' = fromInteger k; n' = fromInteger n

factorPollardRho543b :: Integer -> Maybe (Integer, Integer, Double)
factorPollardRho543b n = factorPollardUsing (\x -> (x^2 + 1) `mod` n) n

factorPollardRho543c :: Integer -> Maybe (Integer, Integer, Double)
factorPollardRho543c n = factorPollardUsing (\x -> (x^2 + 2) `mod` n) n

