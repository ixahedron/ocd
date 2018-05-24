import Lib.EllipticCurve hiding (dadd)
import Lib (eGCD)
import Data.List (find, group)
import Data.Maybe (listToMaybe)

-- 6.8

{-

λ> ecdlpTrialError (ELPF e (4,2)) (ELPF e (0,1))
5

-}

--ecdlp p q = n <=> q = np
ecdlpTrialError :: ELPF -> ELPF -> Maybe Integer
ecdlpTrialError p q = listToMaybe [n | n<-[1..], q == n *^ p]

-- 6.9

{-

Allright, let's write:
  n = is+r, i∈ℤ, s>0, 0<=r<s.

Then
  Q = (is+r)P = isP + rP = O + rP = rP

If r = n0, we're done. But suppose r > n0. Then let's examine Q-Q:

  O = Q - Q = rP - n0 * P = (r - n0)P

So r-n0 > s, right? But s must be the smallest solution to kP = O,
so contradiction. So n0 = r really.
QED

-}

-- 6.10

{-

We want to solve Q = nP.
Using the Basis Problem algo, express both points as:

  Q = k1*B1 + k2*B2
  P = l1*B1 + l2*B2

and substitute in:

  Q = nP
  k1*B1 + k2*B2 = n(l1*B1 + l2*B2)
  (n*l1-k1)B1 + (n*l2-k2)B2 = O

Hmmmmm... {B1,B2} is a basis, of course, so if we have m1*B1 + m2*B2 = O, it must
be that m1 = 0 and m2 = 0, since otherwise, B1 and B2 would be linearly dependent (right?)
Next, we solve:

  n*l1-k1 = 0

From the Basis algo we know both k1 and l1, so we just take n = k1 * l1^(-1) in F_p
and we've solved the problem.

-}

-- 6.11

{-

λ> ex611 23 13 83 (24,14) 19
EP (24,69)
λ> ex611 143 367 613 (195,9) 23
EP (485,573)
λ> ex611 1828 1675 1999 (1756,348) 11
EP (1068,1540)
λ> ex611 1541 1335 3221 (2898,439) 3211
EP (243,1875)

-}

-- Double-and-add algo for elliptic curves
dadd :: ELPF -> Integer -> ELPF
dadd p n = dadd_aux n p O
  where dadd_aux 0 _ r = r
        dadd_aux n q r | odd n = dadd_aux (n `div` 2) (q +^ q) (r +^ q)
                       | otherwise = dadd_aux (n `div` 2) (q +^ q) r

ex611 a b p (x,y) n = let e = ElCurveF a b p; c = ELPF e (x,y) in putStrLn . showReadable $ dadd c n

-- 6.12

{-

λ> mapM_ compareExps [349,9337,38728,8379483273489]
bin: 6; tern: 5
bin: 7; tern: 5
bin: 7; tern: 6
bin: 21; tern: 11

-}

binExpansion :: Integer -> [Integer]
binExpansion 0 = [0]
binExpansion n = (n `mod` 2) : binExpansion (n `div` 2)

ternExpansion :: Integer -> [Integer]
ternExpansion = concat . replaceRun . shift . group . binExpansion
  where replaceRun [] = []
        replaceRun ([]:xs) = replaceRun xs
        replaceRun (zs@(0:_):xs) = zs : replaceRun xs
        replaceRun ([1,0]:xs) = [1,0] : replaceRun xs
        replaceRun (run@(1:_):xs) = let rp = (-1) : replicate (length run - 2) 0 ++ [1] in rp : replaceRun xs

        shift [] = []
        shift zs@[0:_] = zs
        shift (zs@(0:_):xs) = zs:shift xs
        shift [ones@(1:_), [0]] = [ones ++ [0]]
        shift (ones@(1:_):(0:zs):xs) = (ones++[0]) : zs : shift xs

-- multiply out the ternary expansion into hopefully the original integer
testTern :: [Integer] -> Integer
testTern ns = aux ns 0 0
  where aux [] _ s = s
        aux (n:ns) k s = aux ns (k+1) $ s + (n * 2^k)

compareExps n = let bin = length $ filter (/= 0) (binExpansion n);
                    tern = length $ filter (/= 0) (ternExpansion n)
                in putStrLn $ "bin: " ++ show bin ++ "; tern: " ++ show tern


-- 6.13

{-

Oh, we also need a number N = |E|.

Again, what we need is to find such an n that Q = nP. For Pollard's method, we also
need to find an appropriate f for mixing up elements. Let's snatch everything from
Subsection 5.5.2, why not?

f(L) = { P+L if 0 <= x_L < p/3
       { 2L  if p/3 <= x_L < 2p/3
       { Q+L if 2p/3 <= x_L < p

So our function values for i steps and 2i steps will look like
  f_i(L) = α_i*P + β_i*Q, f_2i(L) = γ_i*P + δ_i*Q,

where we know the values of α, β, γ, δ (computed modulo N).

If we find a match (again, after approx. √N steps), then

  α_i*P + β_i*Q = γ_i*P + δ_i*Q <=> (α_i-γ_i)*P = (δ_i-β_i)*Q

If gcd(N, δ_i-β_i)=1, we're in incredible luck and multiply both sides by
(δ_i-β_i)^(-1) to solve the problem. Otherwise we do the same procedure as in normal Pollard's
algo, where we divide by that gcd, solve the resulting equation and then check each of gcd's
possible solutions.

-}

data PollardExps = PExp {α :: Integer, β :: Integer, γ :: Integer, δ :: Integer}
  deriving (Show)

ecdlpOnRhoMatch :: ELPF -> ELPF -> Integer -> PollardExps -> Maybe Integer
ecdlpOnRhoMatch q1@ELPF{..} q2 n PExp{..} = find (\l -> l *^ q1 == q2) lhs
  where a = (α-γ) `mod` n; b = (δ-β) `mod` n;
        (s,_,d) = eGCD b n; w = ((a * s) `mod` n) `div` d
        lhs = [w + k*(n `div` d) | k<-[0..d-1]]

--ecdlpPollardRho P Q = n <=> Q = nP
ecdlpPollardRho :: ELPF -> ELPF -> Maybe Integer
ecdlpPollardRho q1 q2 = let (y,γ,δ) = fRho (q1,1,0);
                        in ecdlpOnRhoMatch q1 q2 n $ rhoxps q1 y $ PExp 1 0 γ δ
  where rhoxps x y exps | x == y = exps
        rhoxps x y PExp{..} = let (x',α',β') = fRho (x,α,β);
                                  (y',γ',δ') = fRho . fRho $ (y,γ,δ)
                              in rhoxps x' y' $ PExp α' β' γ' δ'

        n = curveSize . curve_ $ q1

        fRho :: (ELPF, Integer, Integer) -> (ELPF, Integer, Integer)
        fRho (q,α,β) | p'/3 > x' && x' >= 0 = (q +^ q1, (α+1) `mod` n, β)
                     | 2*p'/3 > x' && x' >= p'/3 = (2 *^ q, (2*α) `mod` n, (2*β) `mod` n)
                     | p' > x' && x' >= 2*p'/3 = (q +^ q2, α, (β+1) `mod` n)
          where x' = if q == O then 0 else fromInteger . fst . q_ $ q;
                p' = fromInteger . p_ . curve_ $ q1

