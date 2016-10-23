-- 1.43

{-
(a) 34 * 204 + 71 mod 541 = 515,
Prelude> (inv 34 541) * (431-71) `mod` 541
297

(b) 204 515, 297 431
With at least two pairs, we can try to solve an equation system.

  k1 * 204 + k2 = 515 mod 541
  k1 * 297 + k2 = 431 mod 541

Using modular arithmetics' rules and subtracting one equation
from another, we get

  k1 * (204-297) = 515-431 mod 541 <=> k1 * (-93) = 84 mod 541

Using modular multiplication rule, it follows that

k1 = (-93)^-1 * 84 mod 541

in this case, k1 = 477 * 84 mod 541 = 34

Then we use that to find k2:

k2 = 515 - (34 * 204 mod 541) mod 541 = 515-444 mod 541 = 71

Quickly rewrite that in general form:

k1 * m1 + k2 = c1 mod p
k2 * m2 + k2 = c2 mod p

=>

k1 * (m1-m2) = c1-c2 mod p <=>
k1 = (m1-m2)^-1 * (c1-c2) mod p

k2 = c1 - (k1 - m1 mod p) mod p

So 2 plain/ciphertexts' pair are enough.

(c) boring

(d) Yes, presumably, because p is the finite space size of that cipher.
So if we have a large number of pairs, we can estimate p, because
no ciphertext can be larger than p-1, and then proceed with trial-and-error
by the algorithm above until achieving success. p pairs give that result,
since encryption is one-to-one, although we don't know how many that is at this point.

-}

-- 1.44

{-

(a) 

(i)    1 3  *  2  +  5  =  5  +  5  =  3  mod 7
       2 2     1     4     6     4  =  3  mod 7


(ii)   1 3 ^-1      = 5      2 -3    3 6
       2 2      = (-4)^-1 * -2  1  = 4 5

(iii)  c = (0 4)^T


(b) I assume it's vulnerable in the same way as affine cipher,
because the same lineality is still present? We'd need at least 1
plain/ciphertext pair for each of the elemets of the k1 matrix
of size nxn, so n^2 pairs, to find out k1, and then another n
pairs to find out k2.

(c) too lazy for linear equations

(d) If we use an alphabet of n symbols, then, to produce a new alphabet,
we obviously need to use an nxn k1 matrix. Further, we'll use
a null vector as k2, because we don't need shifts, only permutations
(for Caesar shift, we can also use the identity matrix for k1 and the
desired shift for k2). Then construct k1 as a matrix where all of the
elements are zeroes, except at places m_i,j, where i and j are
corresponding letters in our permutation. To illustrate, let's use

sigma_1 := {a, b, c, d} := {1,2,3,4}
Suppose the desired perutation is sigma_2 := {c,b,d,a} or {3,2,4,1}. Then

      0 0 1 0       1       0
      0 1 0 0       2       0
k1 =  0 0 0 1 , m = 3, k2 = 0, and
      1 0 0 0       4       0

              0*1 + 0*2 + 1*3 + 0*4   3
                  ...                 2
k1 * m + k2 =     ...               = 4
                  ...                 1

Note that this only constructs a new alphabet, to actually encode a message m,
we'll need an l x n matrix where l := |m|.

-}

-- 1.45

{-

Encryption function is a) one-to-one and b) inversible.

(a) ek is not an encryption function, since

  k - m = k - m + 1 - 1 = (k+1) - (m+1) mod N

which makes it not one-to-one.

One cannot make it into an encryption function, since the property above
depends neither on the size of k nor on the size of m.

(b) ek is not an encryption function in general, since it can happen to be the case
that k will not be coprime with N, and then the absence of modular inverse contradicts the
invertibility condition. Using only keys that are coprime with N makes ek invertible and one-to-one.
The decryption function is then k^-1 * c mod N.

(c) ?

-}

-- 1.47

{-

(a) Prelude> 2^55 / (10000000000 * 3600 * 24)
41.69999654972681

(b) Prelude> logBase 2 $ 100 * 365.25 * 24 * 3600 * 10000000000
64.77462129339004
so B = 65

-}

-- 1.48

{-

if

c = k XOR m,

then immediately

k = c XOR m,

so even one known pair suffices.


1001010001010111
0010010000101100
---------------- XOR
1011000001111011 - is the key

-}

-- 1.49

{-

(a) sqrt 11 = 3.3166247903554 =>
a = 316624

*Main> 316624 + 328973 `mod` 10^6
645597

(b) a = 79583152

*Main> 78183903 - 79583152 `mod` 10^8
-1399249
*Main> it + 10^8
98600751

(c)

l := sqrt k - floor sqrt k leaves only the fractional part of k.
10^d * l shifts the decimal point right, leaving us with exactly d positions
in the whole part of the number and the rest of l in the fractional part.
If we now floor the result, we'll get d first decimal positions of l,
which corresponds to first d decimal positions of k, which is alpha by definition.




-}

-- 1.50

{-

Prelude> gcd 12849217045006222 6485880443666222
174385766

174385766 isn't prime, so let's factor it:
174385766 = 2 * 87192883
so our large prime k is 87192883.


-}
