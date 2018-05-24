import Lib.EllipticCurve
import Lib ((≡), mexp, inv)
import Data.List (find)

-- 6.14

{-

(a) λ> showReadable $ ecdhSend p614a 1943
"EP (1432,667)"
(b) λ> showReadable $ ecdhSend q614a 1943
"EP (2424,911)"
(c) λ> bforceSecretN  p614a q614a
Just 726
(d) λ> ecdhSendX ec614 2 875
1708

-}

ecdhSend :: ELPF -> Integer -> ELPF
ecdhSend p n = n *^ p

bforceSecretN :: ELPF -> ELPF -> Maybe Integer
bforceSecretN qP@(ELPF ElCurveF{..} _) qA = find (\n -> n *^ qP == qA) [2..p_]

ecdhSendX :: ElCurveF -> Integer -> Integer -> Integer
ecdhSendX c@ElCurveF{..} xA nB = fst . q_ $ nB *^ ELPF c (xA, yA)
  where yA = sqrtFin p_ $ (mexp p_ xA 3 + a_ * xA + b_) `mod` p_

sqrtFin :: Integer -> Integer -> Integer
sqrtFin p a | p ≡ 3 $ 4 = mexp p a' ((p+1) `div` 4)
            | otherwise = head [b | b<-[1..p-1], mexp p b 2 == a'] -- yeah yeah, unsafe, inefficient
  where a' = a `mod` p

ec614 = ElCurveF 171 853 2671
p614a = ELPF ec614 (1980,431)
q614a = ELPF ec614 (2110,543)

-- 6.15

{-

p, E = X^3 + aX + b are public knowledge.

Let Alice choose her message = P \in E(F_p).
1. She chooses a secret multiplier nA, computes qA = nA*P and sends it to Bob.
2. Bob chooses a secret multiplier nB, computer qB = nB*qA, sends it back to Alice.
3. Alice sends to Bob pB = nA^(-1)*qB = nB*P.
4. Bob uncovers P by computing nB^(-1)*pB.

All secret multipliers are obvs chosen to have gcd = 1 with N, the order of P.
Same as the original cryptosystem in 2.10, this system is very susceptible
to (EC)DLP attacks.

-}

-- 6.16

{-

(a) Alice computes Y^2 according to the formula. Now this Y^2 has two square roots.
We can't really know what they are immediately, but we know that if one is z,
the other one is -z ≡ p-z ((p-z)^2 = p^2 - 2pz + z^2 ≡ z^2 (mod p)).
Ergo, one of the square roots is necessarily in the [0;1/2p] range, the other
one in the [1/2p;p] one, just need to choose the correct one.
The single additional bit takes care of that.
(b)
λ> showReadable $ singleBit ec616 278 False
"EP (278,487)"
λ> showReadable $ singleBit ec616 278 True
"EP (278,636)"

-}

singleBit :: ElCurveF -> Integer -> Bool -> ELPF
singleBit c@ElCurveF{..} x b = let f = if b then max else min;
                                   z = sqrtFin p_ $ mexp p_ x 3 + a_ * x + b_
                               in ELPF c (x,f z $ p_ - z)

ec616 = ElCurveF 54 87 1123

-- 6.17

{-
                                       *
(a) m1' = xT^-1 * c1 = xT^-1 * xS * m1 = m1
*: T = nA*R = nA * k * P = k * qA = S
Same for m2' = m2

(b) I suppose it's 2, since we have R which is the same as the original P,
and then c1 and c2, which constitute another two numbers modulo p.
Of course, you can send away only the x coordinate of R, that will make the
expansion rate 1.5 (if I may deem an additional single bit negligeable).

(c)
λ> showReadable $ pubkMVElgamal p617c 595
"EP (1104,492)"
λ> decMVElgamal 595 (r617c,279,1189)
(509,767)

-}

pubkMVElgamal :: ELPF -> Integer -> ELPF
pubkMVElgamal p nA = nA *^ p

decMVElgamal :: Integer -> (ELPF, Integer, Integer) -> (Integer, Integer)
decMVElgamal nA (r,c1,c2) = (m1,m2)
  where t@ELPF{..} = nA *^ r
        m1 = (inv (fst q_) p * c1) `mod` p
        m2 = (inv (snd q_) p * c2) `mod` p
        p = p_ curve_

ec617 = ElCurveF 19 17 1201
p617c = ELPF ec617 (278,285)
r617c = ELPF ec617 (1147,640)

-- 6.18

{-

(a)
c1 = m1 * xS <=> xS = c1 * m1^-1
c2 = m2 * yS <=> yS = c2 * m2^-1

So S = (c1 * m1^-1, c2 * m2^-1). Remember that Eve knows the curve we're
working in! So look:
  Y^2 = X^3 + aX + b => (c1 * m1^-1)^3 + a(c1 * m1^-1) + b =
  = c1^3 * m1^-3 + a*c1 * a*m1^-1 + b = c2^2 * m2^-2

Eve knows all the parameters of this polynomial except m1 and m2. If we now
allow her to find out one or the other, she easily solves the equation and
gets both ciphertexts.

(b) λ> breakMVElgamal (r618b,814,1050) 1050
179

-}

breakMVElgamal :: (ELPF,Integer,Integer) -> Integer -> Integer
breakMVElgamal (r,c1,c2) m1 = m2
  where m2 = inv (sqrtFin p_ $ (mexp p_ z 3 + a_*z + b_ ) * inv (mexp p_ c2 2) p_) p_
        ELPF{..} = r; ElCurveF{..} = curve_
        z = c1 * inv m1 p_ -- c1/m1

r618b = ELPF ec617 (269,339)

-- 6.19

{-

E, p and G are published. Say G is of order N = |E|.

·Generating a pubkey:
  Alice selects her private key s from [1;N-1] and computes and publishes
  qA = sG, her public key.

·Sign a doku:
  ··Choose a document D (mod N).
  ··Choose a random element k (mod N).
  ··Compute S1 = kG = (xS,yS).
  ··Compute S2 = (D+s*xS)k^-1 (mod N)
  ··Publish the signature (S1,S2).

·Verify a signature:
  ··Compute xS*qA + D*G
  ··Verify that this is equal to S2*S1.

This is pretty much a straightforward adaptation of Table 4.2 except for
the verification step, where we swap S2*S1 and D*G. I don't know why it had
to be swapped inherently, but this way math checks out and in the original
way it didn't. ¯\_(ツ)_/¯

  xS*qA + D*G = xS*s*G + D*G = (xS*s+D)G
  S2*S1 = (D+s*xS)k^-1 * kG = (D+s*xS)G

Sorry, I can't analyse security. It's beyond my intuhlectual abilities :/
But seems fine since it's such a direct rip-off of the original system?

-}

-- 6.20

{-

(a)
λ> showReadable $ pubkECDSA g620 542
"EP (8689,1726)"
λ> signECDSA g620 1321 542 644 847
(491,290)
(b) λ> verifyECDSA g620 1321 v620b 993 (907,296)
True
(c) s = λ> head [s | s<-[2..17389], s *^ g620 == v620c ]
1294
λ> signECDSA g620 1321 1294 516 365
(1281,236)

-}

pubkECDSA :: ELPF -> Integer -> ELPF
pubkECDSA g s = s *^ g

signECDSA :: ELPF -> Integer -> Integer -> Integer -> Integer -> (Integer,Integer)
signECDSA g q s d e = let s1 = (fst . q_ $ e *^ g) `mod` q;
                          s2 = (inv e q * (d + s * s1) `mod` q)
                      in (s1,s2)

verifyECDSA :: ELPF -> Integer -> ELPF -> Integer -> (Integer, Integer) -> Bool
verifyECDSA g q v d (s1,s2) = let v1 =  (d * inv s2 q) `mod` q;
                                  v2 = (s1 * inv s2 q) `mod` q
                              in (fst . q_ $ (v1 *^ g) +^ (v2 *^ v)) ≡ s1 $ q

ec620 = ElCurveF 231 473 17389
g620  = ELPF ec620 (11259,11278)
v620b = ELPF ec620 (11017,14637)
v620c = ELPF ec620 (14594,308)
