import qualified Data.Map.Lazy as M
import Data.List (nub)

-- 5.45

{-

I would skip it but it's also needed for another task down below, so THERE.
:/

(a) λ> map fC [1..4]
[0.3333333333333333,0.3333333333333333,0.2222222222222222,0.1111111111111111]

(b) λ> map (fCIM 1) [1..3]
[0.3333333333333333,0.3333333333333333,0.3333333333333333]
The system does not have perfect secrecy, e.g. fC|M(c4|m1) = 0, f(c4) = 1/9.

(c) λ> map (flip fCIM 1) [2,3]
[0.3333333333333333,0.3333333333333333]

(d) λ> map (flip fKIC 3) [1,2]
[0.0,0.5]

-}

type Message = Integer
type Key = Integer
type Cipher = Integer

-- ¯\_(ツ)_/¯
kmc545 :: M.Map (Message, Key) Cipher
kmc545 = M.fromList [((1,1),2),((2,1),4),((3,1),1),
                     ((1,2),1),((2,2),3),((3,2),2),
                     ((1,3),3),((2,3),1),((3,3),2)]

ek :: Message -> Key -> Cipher
ek m k = M.findWithDefault 0 (m,k) kmc545

fM :: Message -> Double
fM 1 = 0.4
fM 2 = 0.4
fM 3 = 0.2
fM _ = error "out of message sample space"

fK :: Key -> Double
fK k | k > 3 = error "out of key sample space"
     | otherwise = 1/3

fC :: Cipher -> Double
fC c = let cts = M.elems kmc545 in (length . filter (== c) $ cts) /// length cts

(///) :: Integral n => n -> n -> Double
a /// b = (fromIntegral a) / (fromIntegral b)

-- naming is a v3ry smar7 h4ck for C|M as you can see
fCIM :: Cipher -> Message -> Double
fCIM c m = let condCs = map (\mk -> M.findWithDefault 0 mk kmc545) . map (m,) $ [1..3]
           in (length . filter (== c) $ condCs) /// length condCs

fKIC :: Key -> Cipher -> Double
fKIC k c = let condKs = map snd . nub . M.keys . M.filter (== c) $ kmc545
           in (length . filter (== k) $ condKs) /// length condKs

-- 5.46

{-

(a) |M| = |K| = |C| = 26
Furthermore, it follows from the definition of a shift cipher that
for a fixed c decrypting with different keys cannot result in two
equal m's, since if c + k = m = c + k' (mod 26), then k = k'.
So for an arbitrary but fixed c we have
  ∑_k∈K f_M(d_k(c)) is equivalent to ∑_m∈M f_M(m)

We know that ∑_m f(m) = 1 by definition of a cryptosystem. So we're done.

                                 ks w/ eq.pr.                 (a)
(b) f_C(c) = ∑_k∈K f_K(k) f_M(d_k(c)) = ∑_k∈K 1/26 f_M(d_k(c)) =>
f_C(c) = 1/26 * 1 = 1/26
So each ciphertext is also used with equal probability.

                     ~inEx.5.50
(c) f(c|m) = f(c,m)/f(m) = f(k,m)/f(m) = f(k)f(m)/f(m) = f(k) = 1/26 = f(c)

So indeed we have proved perfect secrecy.

-}

-- 5.47

{-

Ex.5.23(d) states:
  P(E) = ∑_i=1..n P(E|Fi)*P(Fi), where ∪_i=1..n(Fi) = Ω, Fi's disjoint.

Let's say E := "C = c".
Naturally, K' := "k∈K such that c∈e_k(M)" can be decomposed in relevant
F_i's with F_i = {k_i}.
P(Fi) ~= f(k_i) in this case. So we only have to reconcile P(E|Fi) with
fM(d_k(c)).

But f_M(d_k(c)) ~= P(M = m := d_k(c)) should be equal to f_C(e_k(m)), since
we just do the inverse operation: f(c|k,m) instead of f(m|c,k).
But actually for this we should use f(c|k,M) since we sum over every possible
message. With this, f(c|k,M) = f(c|k).

Now just a straightforward plug-in of numbers into the decomposition formula:

  P(C = c) = fC(c) = ∑_i=1..n fC|K(c|k) * fK(k) = ∑_i=1..n fM|K(m|k) * fK(k)
  = ∑_i=1..n fM(d_k(c)) * fK(k)

-}

-- 5.48

{-

e_k must be injective, otherwise it's not invertible --- you can't
decrypt stuff. We know that for finite sets with |A| = |B| injectivity,
surjectivity and bijectivity all imply each other. It's also obvious
that what we're trying to prove is equivalent to the notion of a
bijective encryption function. So we're done.

-}

-- 5.49

{-

If the intersection isn't ∅, that means that the same key encrypts
the same message to two different ciphertexts non-deterministically.
Hence if k∈S_{m,c}∩S_{m,c'} and e_k(m) = c = c', c = c' or the system
doesn't make sense.

-}

-- 5.50

{-

The part about ciphertext was indeed proved in the textbook. The other
part is analogous:
      5.46
  f(c) = f(c|m) = f(c,m)/f(m) = f(c,k)/f(m) = f(c)f(k)/f(m)
  <=> 1 = f(k)/f(m) <=> f(m) = f(k)

And summing over k's we obtain the desired result.

-}

-- 5.51

{-

|M| = |K| = |C| =: n

We follow the way we used to prove the perfect secrecy for the shift cipher
in 5.46:

1. From the property (b) we know that
  e_k(m) = c = e_k'(m) => k = k'

and from Ex.5.48 we know for any fixed k and c there only exists one m.
Combine this and we again get

  ∑_k∈K f_M(d_k(c)) is equivalent to ∑_m∈M f_M(m) which is equal to 1.

Each key from K is used with equal probability, so let's compute the f_C
density function with that in mind:

      5.47
  f(c) = ∑_k∈K f_K(k) f_M(d_k(c)) = 1/n * ∑_k∈K f_M(d_k(c)) = 1/n


  f(c|m) = f(c,m)/f(m) = f(k,m)/f(m) = f(k)f(m)/f(m) = f(k) = 1/n = f(c)

Which by definition means our cryptosystem is indeed perfectly secure.

-}

-- 5.52

{-

Successive choices by definition means X_n^r = X_n followed by X_n^{r-1}.
We can use this to expand H(X_n^r) as follows (w/ X as X_n to avoid clutter):

  H(X^r) = H(X) + ∑_i=1..n P(X) H(X^{r-1}) = H(X) + 1/n * n * H(X^{r-1}) = 
  H(X) + H(X^{r-1})

We repeat this expansion step further r-1 times, getting in the end

                        x r-3
  H(X^r) = H(X) + H(X) + ... + H(X) = rH(X)

QED

-}

-- 5.53

{-

  ∑_i=1..n p_i * log(p_i) + ∑_i=1..n p_i * ∑_j=1..m_i q_ij log(q_ij) =

  ∑_i=1..n p_i * ∑_i=1..n log(p_i) + ∑_i=1..n p_i * ∑_j=1..m_i q_ij log(q_ij) =

  1 * ∑_i=1..n log(p_i) + 1 * ∑_j=1..m_i q_ij * ∑_j=1..m_i log(q_ij) =
  
  ∑_i=1..n log(p_i) + 1 * ∑_j=1..m_i * log(q_ij) =
  
  ∑_i=1..n ∑_j=1..m_i (log(p_i) + log(q_ij)) = 
  
  ∑_i=1..n ∑_j=1..m_i log(p_i * q_ij) = (the same logic as before, see the hint)
  
  ∑_i=1..n ∑_j=1..m_i p_i q_ij log(p_i * q_ij)

-}

-- 5.54

{-

Такc-такc блядь, што тут у нас, матан какой-то ахаха.........

-}

-- 5.55

{-

The inequality:
  ∑_i=1..n α_i F(t_i) ≤ F(∑_i=1..n α_i*t_i)

Base: n = 1: α_1 = 1 => F(t) = F(t) ✓
      n = 2: the inequality is precisely the definition of concavity ✓
Assumption: for n∈ℕ the inequality holds.
Step: n → n+1

Sadly we can't just use the usual "add the new term" induction directly,
because α_1 + ... + α_n already sums up to 1 and negative α's are not
permissible. I'm not yet sure how to tackle that TBH :/

-}

-- 5.56

{-

(a)

By definition of equivocation:

  H(X|Y) = -∑_i=1..n ∑_j=1..m fY(y_j) * fX|Y(x_i|y_j) * log(fX|Y(x_i|y_j))
 indep.
   = -∑_i=1..n ∑_j=1..m fY(y_j) * fX(x_i) * log(fX(x_i))
   = (∑_j=1..m fY(y_j)) * (-∑_i=1..n * fX(x_i) * log(fX(x_i)))
   = (∑_j=1..m fY(y_j)) * H(X)
   =          1         * H(X)
   = H(X)

(b) Yes, don't have the proof 

-}

-- 5.57

{-

I'm going to at least assume that K and M are independent, since otherwise I
understand fuck-all about how this should be proved. (See *)
Also, we again use the fact that K and M uniquely determine C; and K and C
uniquely determine M. Sadly the same can't be said about M and C determining K,
since we can't assume perfect secrecy. But it'll have to do.

So H(X,Y) = H(Y) + H(X|Y), analogous to normal probabilities.

Let's combine K and M into a new virtual random variable, and get the following
expansion:

  H(C,K,M) = H(K,M) + H(C|K,M)

Now notice what the second factor on the right-hand side says: basically H(C|K,M)
is the amount of uncertainty in which ciphertext we'll get if we condition on
a key and on a plaintext.
Well, it's zero. The c is uniquely determined in this case. So:
                    *
  H(C,K,M) = H(K,M) = H(K) + H(M)

Now let's do the same for K,C and compute

  H(M,K,C) = H(K,C) + H(M|K,C)

The second factor on the right-hand side now describes the amount of uncertainty
for the plaintext for given key and ciphertext, which is again 0. So:

  H(M,K,C) = H(K,C) <-- we can't expand here as we did with K,M,
                        K might be not independent from C :(


With this, compute while handwaving ferociously (won't work otherwise):

  H(K|C) = H(K,C) - H(C) = H(M,K,C) - H(C) = H(K,M) - H(C) = H(K) + H(M) - H(C)

-}

-- 5.58

{-

H(K) = λ> h fK [1..3]
1.584962500721156
H(M) = λ> h fM [1..3]
1.5219280948873621
H(C) = λ> h fC [1..4]
1.8910611120726526

H(K|C) = H(K) + H(M) - H(C) per Ex.5.57, so:
λ> h fK [1..3] + h fM [1..3] - h fC [1..4]
1.2158294835358654

-}

h :: (Integer -> Double) -> [Integer] -> Double
h f xs = -(sum . map (\x -> f x * logBase 2 (f x)) $ xs)

-- 5.59

{-
                               ∑≠0            ∑≠0    =>        ∑=0        
  H(K|C) = -∑_i=1..n ∑_j=1..m fC(c_j) * fK|C(k_i|c_j) * log(fK|C(k_i|c_j)) := 0

  => log(fK|C(k_i|c_j)) = 0 for all relevant i,j => fK|C(k_i|c_j) = 1

Which means exactly that we uniquely determine the key from a single ciphertext.

QED

-}

-- 5.60
-- TBH I can hardly be bothered with this, maybe later
