import Lib (mexp)
import Data.List (find)
import Data.Maybe (isJust)

-- 3.37

{-

(a) by FLT, a^(p-1) === 1 (mod p)
Rewrite that using the squares' difference formula:
(I think it's based on fields' properties?
Anyway, Z/p is a field, so we're alright)

  a^(p-1) - 1 === 0 (mod p) <=>
  (a^((p-1)/2) - 1) (a^((p-1)/2) + 1) === 0 (mod p)

So either a^(p-1)/2 is congruent mod p to 1 or -1, QED

(b)
=>: p odd prime => there exists a primitive root g
with a === g^i, i in Z.

  g^(i(p-1)/2) === 1

So I'm too lazy to search for the name of the theorem,
but there was something about order of g p-1 | (i(p-1))/2, right?
Then it must be that i = 2j, j in Z. So i is even and g^i is
a quadratic residue.

<=: 
  a === x^2 (mod p)
  a^((p-1)/2) === x^(p-1) === 1 (mod p)
  
QED

(c) By (b), the (1) part is already proved.
If p∤a, the (-1) part follows from (a) and (b).
So let's quickly deal with p|a:

  p|a <=> a = kp, k in Z <=> a === 0 (mod p)
  0^((p-1)/2) === 0 (mod p), which corresponds to (a/p).

QED

(d) (-1/p) = (-1)^((p-1)/2) by (c).

Let p === 1 (mod 4).
Then p-1 = 4k, k in Z, and (p-1)/2 = 2k, k in Z.
So 2k is even, and (-1)^2k = 1.

Now let p === 3 (mod 4).
Then p-1 === 4k + 2, k in Z, so (p-1)/2 = 2k+1, an odd number.
So (-1)^(2k+1) = -1. QED

-}

-- 3.38

{-

(a) directly follows from 3.37(c)

(b)
let p === 1 mod 8 <=> p = 8k+1, k in Z, so (a/p) = 1.
(-1)^((p-1)(p+1)/8) = (-1)^(8k(8k+2)/8) = (-1)^(4k*(8k+2))
So it's an even power resulting in 1.

let p === 3 mod 8 <=> p = 8k+3, k in Z, so (a/p) = -1.
(-1)^((8k+3)^2-1)/8 = (-1)^(64k^2+48k+8)/8 = (-1)^(8k^2+6k+1)
which yields an odd power resulting in -1.

let p === 5 mod 8 <=> p = 8k+5, k in Z, so (a/p) = -1.
(-1)^((8k+5)^2-1)/8 = (-1)^(8k^2+10k+3)
again, an odd power => -1

let p === 7 mod 8 <=> p = 8k+7, k in Z, so (a/p) = 1.
(-1)^(64k^2+112k+48)/8 = (-1)^(8k^2+14k+6)
an even power yielding 1.
QED

(c)
If p = 4k+1 OR q = 4j+1:
(-1)^(p-1)(q-1)/4 = (-1)^2k((q-1/2)) OR
(-1)^(p-1)(q-1)/4 = (-1)^(((p-1)/2)*2j)
both of which are even powers resulting in 1.
According to 3.62(c), in this case (p/q) = (q/p),
so their product also results in 1.

Now, if p = 4k+3 AND q = 4j+3:
(-1)^((4k+2)/2)*((4j+2)/2) = (-1)^(2k+1)(2j+1)
a product of odd numbers is odd, so the result is -1.
According to 3.62(c), (p/q) = -(q/p), so their product is -1.
QED

-}

-- 3.39

{-

(a) Adhering to the hint, write          3.37b
  b^2 === a^(p+1 / 2) === a * a^(p-1 / 2) === a, QED

(b)

λ> sqRootMod 587 116
Right 65
λ> sqRootMod 8627 3217
Right 2980
λ> sqRootMod 10663 9109
Left "a is a quadratic nonresidue"

-}

sqRootMod :: Integer -> Integer -> Either String Integer
sqRootMod p a | p `mod` 4 /= 3 = Left "p ≢ 3 mod 4"
              | mexp p b 2 == a = Right b
              | otherwise = Left "a is a quadratic nonresidue"
  where b = mexp p a $ (p+1) `div` 4

-- 3.40

{-

I probably fucked up my understanding of the problem again,
but there seemingly isn't much more to the exercise than
following the hint.

Use Example 3.69 to compute the first bit.
If it's -1, then h is a nonresidue, which we "fix"
by multiplying it with g^-1, which ultimately yields
a quadratic residue. Take the square root of the
resulting quadratic residue, be it h or g^-1 * h, repeat.

To see why it's only possible to find out the first s bits,
let's look at that square root. Let j be a quadratic residue
(either j := h or g^-1 * h). Then we know that j = g^2r from
Proof of 3.61.
So the square root of j is either x1 := g^r or x2 := g^(r+(p-1)/2).
Note that this means that if x = g^r, then x is obtained
from j by shifting 2r one bit to the right.
So we can obtain both square roots and they will differ
in the sth bit, I think? Because
  log_g(y) - log_g(x) = r+(p-1)/2 -r = (p-1)/2 = m*2^(s-1)

(I know I'm not making much sense, I'm unable to read what I
worked out on paper because my handwriting sucks so I'm filling
in the gaps from scratch while falling asleep :/)

So when you hit that differing place, you don't know which one
of the resulting bits is correct, but until that you're fine.
So you can basically rshift it s-1 times. Yeah.
.......
mne_tyazhelo.png

-}

-- 3.41

{-

(b) let's say (a,b) = (3,12):
λ> cubRoot31 3
Nothing
λ> cubRoot31 12
Nothing
λ> cubRoot31 36
Nothing

-}

isCubicRes :: Integer -> Integer -> Bool
isCubicRes p = isJust . cubRoot p

cubRoot :: Integer -> Integer -> Maybe Integer
cubRoot p a = find (\c -> mexp p c 3 == a `mod` p) [1..p-1]
