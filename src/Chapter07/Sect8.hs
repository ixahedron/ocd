import Lib.Lattice
import Lib.Matrix

-- 7.18

{-

(a)
λ> detL b718prv
561
λ> hadamard b718prv
0.7536218249363512
λ> hadamard b718pub
1.1019992284635005e-3

(b)
λ> decrypt b718prv b718pub e718
Vect {vlist = [8.0,3.0]}
λ> e718 `vminus` v
Vect {vlist = [4.0,2.0]}

(c)
λ> decrypt b718pub b718pub e718
Vect {vlist = [-8.0,-23.0]}
not cool

-}

b718prv, b718pub :: Num a => Matrix a
b718prv = matrix [[4,13],[-57,-45]]
b718pub = matrix [[25453,9091],[-16096,-5749]]
e718 :: Num a => Vect a
e718 = Vect [155340, 55483]

decrypt :: RealFrac a => Basis a -> Basis a -> Vect a -> Vect a
decrypt prv pub e = expressInBase pub $ babai prv e

-- 7.19

{-

(a) λ> detL b719prv
672858
λ> hadamard b719prv
0.6169653190266731
λ> hadamard b719pub
2.9999434812456882e-5

(b) λ> decrypt b719prv b719pub e719
Vect {vlist = [-50.0,-91.0,83.0]}
λ> e719 `vminus` babai b719prv e719
Vect {vlist = [-10.0,-3.0,8.0]}

(c)
λ> decrypt b719pub b719pub e719
Vect {vlist = [52.0,417.0,-159.0]}

-}

b719prv, b719pub :: Num a => Matrix a
b719prv = matrix [[58,53,-68],[-110,-112,35],[-10,-119,123]]
b719pub = matrix [[324850, -1625176, 2734951],[165782, -829409, 1395775],[485054, -2426708, 4083804]]
e719 :: Num a => Vect a
e719 = Vect [8930810, -44681748, 75192665]

-- 7.20

{-

(a) The only thing Eve will be deducing is e-e'=r-r', which in turn can help her deduce r and r'
(b) λ> e720e `vminus` e720e'
Vect {vlist = [-3,-3,3,-2,1]}
That together with the set of coordinates means the possibilities are significantly reduced. So:
  r'1 - 3 ≤ 2-3=-1, so r1 in [-2,-1]
  r'2 - 3 ≤ 2-3=-1, so r2 in [-2,-1]
  r'3 + 3 ≥ -2+3=1, so r3 in [1,2]
  r'4 - 2 ≤ 2-2=0,  so r4 in [-2,-1,0]
  r'5 + 1 ≥ -2+1=-1, so r5 in [-1,0,1,2]
With that, the number of possible rs is 2*2*2*3*4=96

(c) Well, here it's the same, only Eve can narrow down the possibilities for m and m'.

-}

e720e = Vect [-9, -29, -48, 18, 48]
e720e' = Vect [-6, -26, -51, 20, 47]

-- 7.21

{-

(a) Obvious --- hash function and W are publicly available.
(b) If we condition on the hash function not inflating the encryption beyond Alice
being able to decrypt it using the good basis, then Alice can get
  m = (m0 xor r0) || r0.
The dimension of r0 is known, so she can easily invert xor and receive the message.

Now, the problem in (a) is solved, because Eve presumably can't simultaneously correctly
guess both m0 and r0, and she certainly can't check if she's correct without knowing one
or the other with certainty.

The repeated messages are similarly no longer a problem, because M(m0,r0) and M(m0,r0')
result in two completely different 1) messages m; 2) hashes r.
Same with repeated r0s for different messages.

(c) AES?

-}
