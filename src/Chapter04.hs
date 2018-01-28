import Lib (mexp, inv, sbg)
import Data.Numbers.Primes (primeFactors)

-- 4.1

{-

(a)
λ> 541*1223
661643
λ> computePrvSKey pbk prk
PrvSKey {d_ = 561517}

(b)
λ> signRSA pbk it 630579
206484

-}

data PrvEKey = PrvEKey {p_ :: Integer, q_ :: Integer} deriving Show
data PubSKey = PubSKey {n_ :: Integer, e_ :: Integer} deriving Show
data PrvSKey = PrvSKey {d_ :: Integer} deriving Show

computePrvSKey :: PubSKey -> PrvEKey -> PrvSKey
computePrvSKey pubk (PrvEKey p q) = let e = e_ pubk in PrvSKey $ inv e $ (p-1)*(q-1)

-- signRSA pubSKey prvSKey D = S == D^d
signRSA :: PubSKey -> PrvSKey -> Integer -> Integer
signRSA pubk (PrvSKey d) doc = let n = n_ pubk in mexp n doc d

verifyRSA :: PubSKey -> Integer -> Integer -> Bool
verifyRSA (PubSKey n e) s doc = (mexp n s e) == doc

-- 4.2

{-

λ> map (uncurry . flip $ verify pbk) $ zip [119813, 161153, 586036] [876453, 870099, 602754]
[False,True,True]

-}

-- 4.3

{-

λ> let d = let p:q:[] = primeFactors 27212325191 in computePrvSKey pubk (PrvEKey p q)
λ> d
PrvSKey {d_ = 18408628619}
λ> signRSA pubk d 12910258780
22054770669

-}

-- 4.4

{-

s ≡ hash(m)^dA (mod N_A) => s^eA ≡ (hash(m)^dA)^eA ≡ hash(m)
As for why that works: hash functions are difficult to invert and
additionally there aren't many collisions, and N_A is, as always,
difficult to factor, so the chances of Eve finding a valid signature
without knowing Alice's secrets are very low.

-}

-- 4.5

{-

(a)
λ> let ss = EGS 6961 437
λ> let sk = EGSgnK ss 6104
λ> computeEGVerKey sk
EGVerK {..., pA_ = 2065}

(b) λ> signEG sk 5584 4451
(3534,5888)

-}

data ElGamalSS = EGS {pe_ :: Integer, g_ :: Integer} deriving Show
data EGVerK = EGVerK {_pss :: ElGamalSS, pA_ :: Integer} deriving Show
data EGSgnK = EGSgnK {_ss :: ElGamalSS, a_ :: Integer} deriving Show
type EGSignature = (Integer, Integer)

computeEGVerKey :: EGSgnK -> EGVerK
computeEGVerKey (EGSgnK ss@(EGS p g) a) = EGVerK ss $ mexp p g a

signEG :: EGSgnK -> Integer -> Integer -> EGSignature
signEG (EGSgnK (EGS p g) a) d k = let s1 = mexp p g k
                                  in (s1, ((d-a*s1)*(inv k $ p-1)) `mod` (p-1))

verifyEG :: EGVerK -> Integer -> EGSignature -> Bool
verifyEG (EGVerK (EGS p g) a) d (s1,s2) =
    ((pexp a s1) * (pexp s1 s2)) `mod` p == pexp g d
  where pexp = mexp p

-- 4.6

{-

λ> map (uncurry $ verifyEG vk) $ zip [1521, 1837, 1614] [(4129,5575), (3145, 1871), (2709, 2994)]
[True,False,True]

-}

-- 4.7

{-

A^S1 * S1^S2 ≡ (A^S1*A^jS2) * g^(iS2) ≡
A^(S1-S1) * g^iS2 ≡ g^(-S1*i*(j^-1)) ≡ g^D (mod p)
QED

-}

-- 4.8

{-

(a) S1 = g^k (mod p), so S1 will be the same for both documents.
(b) The answer: a ≡ (S2'-S2)^-1(D*S2'-D'S2)S^-1 (mod p-1)

  S2 ≡ (D-aS1)k^-1 <=> a ≡ D*S1^-1 - (S1^-1)*S2*k (mod p-1)
  S2' ≡ (D'-aS1')k^-1, S1=S1' => a ≡ D'*S1^-1 - (S1^-1)S2'*k

Use the usual Gaussian elimination:

  S2'a ≡ D*S2'*S1^-1 - S2*S2'*S1^-1*k (mod p-1)
  S2*a ≡ D'*S2*S1^-1 - S2*S2'*S1^-1*k (mod p-1)

  (S2'-S2)a ≡ (D*S2'-D'*S2)S1^-1

  a ≡ (S2'-S2)^-1(D*S2'-D'*S2)S1^-1 (mod p-1)

Now, we're not guaranteed that any of those inverses will have
gcd(x,p-1)=1, i.e. that it will be possible to take an inverse.
If that's the case, lucky us, we're done. Otherwise, rewrite
that equation for easier reasoning:

   := φ*a
  S1(S2'-S2)a ≡ D*S2'-D'*S2 (mod p-1)

We've already established that gcd(φ,p-1) = c > 1.
That means this equation will have c solutions. If we can get
them, one of them will be equal to a, the secret key.
If the c also divides the right-hand side of the equation, we
can divide by c, solve the resulting equation, then add (up
to c) multiples of p-1/c until we get our private key,
verifiable by looking for g^x ≡ A (mod p).
Unfortunately I don't know how to generalise for when c
doesn't immediately divide the right-hand side (is it
even possible? I assume so, but eh)

(c) λ> a_ <$> recovera vk sg1 sg2
Just 72729

-}

recovera :: EGVerK -> (Integer, EGSignature) -> (Integer, EGSignature) -> Maybe EGSgnK
recovera (EGVerK ss@(EGS p g) dA) (d,(s1,s2)) (d',(s1',s2'))
    | s1 /= s1' = Nothing
    | gcd s1 (p-1) == 1 && gcd (s2'-s2) (p-1) == 1 = Just (EGSgnK ss $ a)
    | ds2 `mod` dvsr == 0 = Just (EGSgnK ss $ a_choice)
    | otherwise = Nothing
  where a = (inv (s2'-s2) (p-1) * inv s1 (p-1) * ds2) `mod` (p-1)
        a_choice = head $ filter (\x -> mexp p g x == dA) aas
          where aa = (ds2 `div` dvsr)*inv (s1s2 `div` dvsr) ((p-1) `div` dvsr)
                aas = map (\x -> (aa+x*((p-1) `div` dvsr)) `mod` (p-1)) [0..dvsr-1]

        dvsr = gcd (p-1) $ s1s2
        ds2 = d*s2' - d'*s2
        s1s2 = s1*(s2'-s2)


-- 4.9

{-

(a) 
λ> let vk = computeDSAVerKey sk
λ> dA_ vk
4940

(b) λ> signDSA sk 244 574
(444,56)

-}

data DSAParameters = DSP {dP_ :: Integer, dQ_ :: Integer, dG_ :: Integer} deriving Show
data DSAVer = DSV {dvPar :: DSAParameters, dA_ :: Integer} deriving Show
data DSASgn = DSS {dsPar :: DSAParameters, da_ :: Integer} deriving Show

computeDSAVerKey :: DSASgn -> DSAVer
computeDSAVerKey (DSS sp a) = let DSP p _ g = sp in DSV sp $ mexp p g a

signDSA :: DSASgn -> Integer -> Integer -> EGSignature
signDSA ss d k = let DSS sp a = ss; DSP p q g = sp;
                     s1 = (mexp p g k) `mod` q;
                     s2 = ((d + a*s1)*inv k q) `mod` q
                 in (s1, s2)

verifyDSA :: DSAVer -> Integer -> EGSignature -> Bool
verifyDSA vkey d (s1,s2) = ((gv1 * av2) `mod` p) `mod` q == s1
  where v1 =  (d * inv s2 q) `mod` q
        v2 = (s1 * inv s2 q) `mod` q
        gv1 = mexp p g v1
        av2 = mexp p a v2

        DSV sp a = vkey
        DSP p q g = sp

-- 4.10

{-

λ> verifyDSA vk 329 (183,260)
True
λ> verifyDSA vk 432 (211,97)
False

-}

-- 4.11

{-

λ> crackDSA vk
DSS {..., da_ = 602}
λ> signDSA sk 510 1105
(439,1259)
-}

-- using Shank's babystep-giantstep algo
crackDSA :: DSAVer -> DSASgn
crackDSA (DSV sp dA) = let DSP p q g = sp in DSS sp $ sbg p q g dA
