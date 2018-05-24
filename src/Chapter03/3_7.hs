import Lib (ln, lnR, e, mexp)
import Data.Numbers.Primes (primeFactors, primes, isPrime)
import Data.List (group, sort, nub)
import Data.List.Ordered (mergeAll)

-- 3.27

{-

λ> psi 3 25
10
λ> psi 5 35
18
λ> psi 7 50
30
λ> psi 5 100
33
λ> psi 7 100
45

-}

-- a number of B-smooth numbers between 2 and x
psi :: Integer -> Integer -> Integer
psi b x | x < 2     = 0
        | otherwise = fromIntegral . length $ filter (isBSmooth b) [2..x]

isBSmooth :: Integer -> Integer -> Bool
isBSmooth b x = let xfactors = primeFactors x in all (<= b) xfactors

-- 3.28

{-

(a) factors of p^e (i.e. p) cannot be larger that p^e,
    (<=) is transitive, basically nothing to prove
(b) not always, see (c)
(c)
λ> filter isSmooth3v28 l3v28
[84,224,378,420,504]
λ> filter isPowSmooth3v28 l3v28
[84,420,504]

(d) the first thing is to notice that lcm [1..B] is just a product
of the highest powers of distinct primes, since lcm a a^e = a^e
Example: lcm [1..10] = 2520 = 8 * 9 * 5 * 7

Let L = lcm [1..B].
<=: M|L <=> L = M*k for some k in Z <=> gcd(M,L) = M.
L is the product of (some powers) of _all_ the primes ≤B, so if M|L,
M doesn't have power factors larger than B, otherwise there would be
some leftover factors on both sides and gcd(M,L) wouldn't be =M.

=>: L is divided by every number up to B, which implies it's also
divided by every M's power factor, since we know none are >B.

QED

-}


l3v28 = [ 84, 141, 171, 208, 224, 318, 325, 366, 378, 390,
          420, 440, 504, 530, 707, 726, 758, 765, 792, 817 ]

isSmooth3v28 = isBSmooth 10
isPowSmooth3v28 = isBPowerSmooth 10

isBPowerSmooth :: Integer -> Integer -> Bool
isBPowerSmooth b x = all (<= b) $ factorPowers x

factorPowers :: Integer -> [Integer]
factorPowers = map product . group . primeFactors

-- 3.29

{-

λ> opsPerTime secs $ fL (2^100)
1
λ> opsPerTime hours $ fL (2^250)
3
λ> opsPerTime days $ fL (2^350)
83
λ> opsPerTime years $ fL (2^500)
1130
λ> opsPerTime years $ fL (2^750)
183322908
λ> opsPerTime years $ fL (2^1000)
5549434339898
λ> opsPerTime years $ fL (2^2000)
179769313486231590772930519078902473361797697894230657273430081157732675805500963132708477322407536021120113879871393357658789768814416622492847430639474124377767893424865485276302219601246094119453082952085005768838150682342462881473913110540827237163350510684586298239947245938479716304835356329624224137216

Bless Haskell's integers

-}

lnL :: Integer -> Double
lnL x = sqrt $ ln x * (lnR . ln $ x)

fL :: Integer -> Double
fL x = e ** lnL x

-- opsPerTime t x gives you opsPerSec for t = 1,
--                          opsPerMin for t = 60 etc
opsPerTime :: Double -> Double -> Integer
opsPerTime t x = ceiling $ (fromIntegral . ceiling $ x/(10^9)) / t

secs = 1
mins = 60*secs
hours = 60*mins
days = 24*hours
years = 365.25*days

-- 3.30

{-

There used to be some shitty holey explanation here,
but now I'll just say it's a special case of 3.31(a).

-}

-- 3.31

{-

For less cluttered notation, exp(x) := e^x

F_a,b(x) := exp( ln(x)^(1/a) * lnln(x)^(1/b)  )

(a) a > 1:

  lim f(x)/g(x) = c < ∞ => f(x) = O(g(x)).
  x->∞

  (i) α>0 => F_a,b(X) = Ω(ln(X)^α)

    ln(x)^α = exp(α*ln(ln(x)))

    lim F_a,b(X) / exp(α*lnlnX) = lim exp( lnX^(1/a) * lnlnX^(1/b) - α*lnlnX ) =
    X->∞                          X->∞

                       ->∞                      ->∞
    exp (lim (lnX^(1/a) * lnlnX^(1/b -1) - α) * lnlnX ) = +∞
         X->∞

  (ii) β>0 => F_a,b(X) = O(X^β)

     lim F_a,b(X) / exp(β*lnX) = lim exp( lnX^(1/a) * lnlnX^(1/b) - β*lnX ) =
     X->∞                          X->∞
                                               *
     exp(lim lnX^(1/a) * lnlnX^(1/b) - β*lnX ) = exp(-∞) = 0
         X->∞

     * lnX^(1/a) * lnlnX^(1/b) - β*lnX =
       lnX^(1/a) * (lnlnX^(1/b) - β*lnX^(1- 1/a)) =

       a>1 => 1-(1/a) >0, lnX > 0, lnlnX - lnX^(>0) < 0
       so their product < 0


     lim F_a,b/X^β = 0 < ∞ => F_a,b = O(X^β).
     X->∞

(b) a = 1:
                                            *
  exp(lim lnX^(1/a) * lnlnX^(1/b) - β*lnX ) = exp(+∞) = ∞
      X->∞

  * a = 1 =>
    lnX * lnlnX^(1/b) - β*lnX = lnX*(lnlnX^(1/b) - β)

    X->∞ => lnlnX^(1/b) > 0, lnX > 0,
    so their product is ∞.

(c) It grows further, so I guess it's still superexponential?

-}


fAB a b x = e**(e1*e2)
  where e1 = ln x ** (1/a)
        e2 = (lnR . ln $ x)**(1/b)



-- 3.32

{-

(a) ln L(X) = √(lnX * lnlnX)

First of all, ε should obviously be less that 1/2 (check the limits).

I think it's somewhat easier to use 1/2-ε, so let's define
  α := 1/2-ε, so that ε = 1/2-α, α in (0,1/2)

(i) (lnX)^(1/2 - α) < √(lnX * lnlnX):

Notice that for all X > e^e every choice of α works:

  lnlnX > 1 => √(lnX * lnlnX) > √lnX =>
  => forall α in (0,1/2) (lnX)^(1/2-α) < lnL(X)

So what happens for 10<X<e^e (~15.15...)?

  lnX^(-α) < √lnlnX => exp(-αlnlnX) < exp(0.5*lnlnlnX)
Compare exponents, shuffle things around and you get:

  α > -1/2 * (lnlnlnX/lnlnX)

That's better, but still too dependent on X. Let's examine
the shape of a function. Wolfram Alpha (or derivatives)
tell us it monotonically decreases until e^e^e, then rises.
At point 10 f has then the largest value and we should take
that as α for all Xs in the needed range.

  α := -1/2 * lnlnln10/lnln10

λ> -0.5 * (ln . ln . ln $ 10)/(ln . ln $ 10)
0.10879850972172765

(ii) √(lnX * lnlnX) < (lnX)^(1/2 + α):
 (ε = 1/2-α => 1-ε = 1/2+α)

Do the same thing as in (i):

  √lnlnX < lnX^α => exp(0.5*lnlnlnX) < exp(αlnlnX)

  α > 0.5 * lnlnlnX/lnlnX

Hey, this plots as the same function as in (i) too,
only flipped vertically. It means it increases until e^e^e,
then decreases. For the largest value just evaluate at e^e^e
(remember for X>e^e every α works!):

  lnlnln(e^e^e) = 1
    lnln(e^e^e) = e

  α > 1/2 * 1/e = 1/2e

Now combine both conditions and take whichever is bigger.
Here it's 1/2e. So the formula is true for every α > 1/2e
with ε = 1/2-α.

QED

(b) Rewriting big expressions like that in ascii one-liners
is a grim experience, so here's a photo of the task done
on paper in my trademark handwriting:

  http://nn.lv/u1cp

-}

-- 3.33

{-

(a) a = ⌊√N⌋ + k => a^2 = ⌊√N⌋^2 + 2k⌊√N⌋ + k^2
⌊√N⌋^2 ≤ N => a^2 - N ≤ 2k⌊√N⌋ + k^2, k ≤ K => QED

(b) Yadda yadda yadda, let's skip to the more general case.

M := N^(1/r)
j := 1/√r


  lim  ln(exp(√(lnM*lnlnM)))/ln(exp(√(lnN*lnlnN))^j) =
  N->∞

  lim  √(lnM*lnlnM) / (j*√(lnN*lnlnN)) =
  N->∞

  lim  √(1/r * lnN*ln(1/r * lnN)) / √(1/r * lnN*lnlnN) =
  N->∞

  lim  √(ln(1/r * lnN)) / √lnlnN = lim √((ln(1/r) + lnlnN) / lnlnN) =
  N->∞                             N->∞

  √(lim  ((ln(1/r) + lnlnN) / lnlnN)) =
    N->∞

  √(lim (ln(1/r) / lnlnN) + lim (lnlnN / lnlnN)) = √(0 + 1) = 1
     N->∞                   N->∞

QED

(c) poka hz((9

-}


-- 3.34

{-

(a) λ> factorBySieve 493 11 [23..38]
Just (29,17)
λ> primeFactors 493
[17,29]

(b)
λ> sieve 493 11 [23..38]
[(23,[2,3,2,3]),(25,[2,3,2,11])]
λ> sieve 493 16 [23..50]
[(23,[2,3,2,3]),(25,[2,3,2,11]),(31,[2,3,2,3,13]),(47,[2,3,2,11,13])]

-}


factorBySieve :: Integer -> Integer -> [Integer] -> Maybe (Integer, Integer)
factorBySieve n maxb ts = combineRels n $ sieve n maxb ts

-- just search for relations that give us squares consisting of
-- small primes by trying every combination PO-TUPOMU
combineRels :: Integer -> [(Integer, [Integer])] -> Maybe (Integer, Integer)
combineRels _ [] = Nothing
combineRels n ((s1,rfs):rs) = comb_aux rs
  where comb_aux [] = let fcs = group . sort $ rfs;
                          q = gcd n $ s1 - (product . half $ fcs)
                      in if sqre fcs && q /= 1 && q /= n
                         then Just (q, n `div` q)
                         else combineRels n rs
        comb_aux ((s2,lfs):ls) = let fcs = group . sort $ lfs ++ rfs;
                                     q = gcd n $ s1*s2 - (product . half $ fcs);
                                 in if sqre fcs && q /= 1 && q /= n
                                    then Just (q, n `div` q)
                                    else comb_aux ls

        sqre = all even . map length
        half = concatMap (\l -> take (length l `div` 2) l)


sieve :: Integer -> Integer -> [Integer] -> [(Integer, [Integer])]
sieve n maxb = sieveBy (map reduceToPrime $ takeWhile (<= maxb) primePowers)
  where
    sieveBy  _     [] = []
    sieveBy ps (t:ts) | mexp n t 2 == product l `mod` n = (t, l) : sieveBy ps ts
                      | otherwise = sieveBy ps ts
      where
        sieveBy_aux  _     [] = []
        sieveBy_aux ft (p:ps) | ft `mod` p /= 0 = sieveBy_aux ft ps
                              | otherwise = p : sieveBy_aux (ft `div` p) ps

        l = sieveBy_aux (f t) ps

    f t = t^2 - n

-- dumb bruteforce solver of equations modulo N
-- solveEq m n = [t] -> t^2 === m (mod n)
solveEq :: Integer -> Integer -> [Integer]
solveEq m n = solve_aux [1..n]
  where solve_aux [] = []
        solve_aux (t:ts) | mexp n t 2 == m `mod` n = t : solve_aux ts
                         | otherwise = solve_aux ts

-- intended for use only with primePowers
-- to achieve [2,3,2,5,7,2,3,11...] for easy canceling
reduceToPrime :: Integer -> Integer
reduceToPrime n | isPrime n = n
                | otherwise = reduceToPrime $  n `div` (head . primeFactors $ n)

-- an infinite list of numbers with 1 unique factor
-- [2,3,4,5,7,8,9,11,...]
primePowers :: [Integer]
primePowers = mergeAll [[p^i | i <- [1..]] | p <- primes]
