import Data.List (sortOn, groupBy)
import Data.Function (on)

{-

2.29

First of all, by comparing needed properties of our commutative rings with an 1 and that of a field,
we see that the only difference lies in multiplicative inverses. So that's what we need to prove.

Recall the definition of injectivity: f(x) = f(y) => x = y for all x,y

We notice that a*b for a /= 0 (the map in the hint) is injective for every fixed non-zero element
(I'm not sure how it should be proven but it seems intuitive: if a /= 0, then a*x = a*y => x = y)

There is also some theorem about inj <=> surj <=> bij in functions over finite sets.

By definition our ring has a multiplicative identity. That together with surjectivity means that
for every non-zero element a there is an element b in R so that a*b = 1,
that means every non-zero element has an inverse, which supplies us with the last needed
property of a field: multiplicative inverses for elements in R\{0}
So R is a field. QED


2.30

(a) Assume there is at least one other additive identity e with 0 /= e. Then 0 = 0 + e = e. Contradiction!
(b) Assume there is at least one other multiplicative identity e with 1 /= e. Then 1 = 1 * e = e. Contradiction!

(c) Assume there is at least different two additive inverses for some a in R, call them v and w.
Then v = 0+v = a+w+v = (commutativity of addition) a+v+w = 0+w = w. Contradiction!

(e) a + (-a) = 0

(g) follows directly from (d)

(h) Let a be an element of R with at least two multiplicative inverses v and w with v /= w.
Then v = 1*v = a*w*v = (commutativity of multiplication) a*v*w = 1*w = w. Contradiction!


2.31

(a) i. Let e = phi(0_R). Take a v in R with w = phi(v). Then w = phi(v) = phi(0_R+v) = phi(0_R)+w = e+w, so phi(0_R) = 0_S.

ii. The same argument as in i. holds for phi(1_R) = 1_S.

iii. 0_S = phi(0_R) = phi(a + (-a)) = phi(a) + phi(-a) = phi(a) + (-phi(a))
iv. 1_S = phi(1_R) = phi(a*(a^-1)) = phi(a) * phi(a^-1) = phi(a) * phi(a)^-1

(b) phi(a+b) = (a+b)^p. This must be equal to a^p + b^p, so we need to try to expand this using binomial coefficients.
The formulas snatched from Wikipedia:

(a+b)^p = sum from k=0 to p of \binom{p}{k} a^(p-k)*b^k

binom{p}{k} = p!/(k!(p-k)!)

The first and the last coefficients are of course equal to 1, but any of those in the middle
must vanish since they are divisible by p, p is prime (which means that (k!(p-k)!) doesn't have p
as one of its divisors) and, as we know, p in the coefficient renders the summand equal to 0.
So indeed phi(a+b) = (a+b)^p = a^p + b^p = phi(a) + phi(b).

Finally, phi(ab) = (ab)^p = a^p*b^p = phi(a)*phi(b) is easy. QED

2.32

Once again using that a === b mod m <=> a = mk + b2, where k \in R, we get:

a1 === a2 mod m <=> a1 = mk + a2
b1 === b2 mod m <=> b1 = ml + b2

a1 + b1 = m(k+l) + a2 + b2 <=> a1 + b1 === a2 + b2 mod m
                                             (R closed)
a1 + b2 = m^2*kl + mkb2 + mla2 + a2b2 = m(mkl + kb2 + la2) + a2b2

QED

2.34

(a) follows from definition of multiplications, I think?

(b) <= is easy, since F is a field.

=>: let a with deg n, n > 0, be a unit in F[x]. Then, by (a), the inverse of a would have to have deg -n.
But, by definition of F[x], deg of a polynomial can only be >= 0.
The only polynomial whose degree's inverse can be >= 0 is constant polynomial with deg = 0.

(c) Base n = 1: is irreducible by itself;

Step: assume every polynomial of degree n can be written as a product of irreducible polynomials.
For every polynomial of degree n+1 we then have two possibilities: either it's already irreducible,
in which case it is its own product, or it can be factored into a number of polynomials of lesser degree.
If the latter is the case, we use the assumption to write out the factors as products of irreducible polynomials.
The product of products is itself then obviously a product of irreducible polynomials.
QED

(d)
(2x^3 + 1) * 3x^4 = 6x^7 + 3x^4 in Z[x], meaning deg a + deg b = 3 + 4 = 7 = deg ab, but
(2x^3 + 1) * 3x^4 = 3x^4 in R[x], meaning deg ab = 4 /= 7 = 3 + 4 = deg a + deg b



2.35 need to write code probably
2.36 too

-}

-- <83, amirite? :)
newtype Poly = P [(Integer, Integer)]

instance Show Poly where
  show (P []) = []
  show (P [(0,_)])   = []
  show (P [(a,0)])   = sgn a ++ show a
  show (P [(1,x)])   = "+x^" ++ show x
  show (P [(a,x)])   = sgn a ++ show a ++ "x^" ++ show x
  show (P ((a,x):p)) = show (P [(a,x)]) ++ show (P p)

sgn a | a >= 0    = "+"
      | otherwise = []

pprint :: Poly -> IO ()
pprint (P p) = print . P . reverse . sortOn snd $ p


{--instance Functor Poly where
  fmap f (P p) = P (f p)

instance Applicative Poly where
  pure p = P p
  (P f) <*> (P p) = P (f p)
-}

add :: Poly -> Poly -> Poly
add p (P []) = p
add (P []) p = p
add (P p1) (P p2) = P . normalize $ (p1 ++ p2)

mult :: Poly -> Poly -> Poly
mult (P []) _ = P []
mult _ (P []) = P []
mult (P p1) (P p2) = P . normalize $ ((\(a,x) (b,y) -> (a*b, x+y)) <$> p1 <*> p2)

normalize :: [(Integer, Integer)] -> [(Integer, Integer)]
normalize = filter (\x -> fst x /= 0) . map (\m -> (sum . map fst $ m, snd . head $ m)) . groupBy ((==) `on` snd) . sortOn snd

instance Num Poly where
  (+) = add
  (*) = mult
  abs = id -- wat
  signum (P p) = P (map (\(a,x) -> (signum a, signum x)) p) -- wat[1]

  negate (P []) = P []
  negate (P p) = P (map (\(a,x) -> (-a,x)) p)

  fromInteger a = P [(a,0)]

pmod :: Integer -> Poly -> Poly -> (Poly, Poly)
pmod p a b | deg a < deg b = pmod p b a
           | otherwise     = pmod_aux (P []) a
  where pmod_aux k r | deg r < deg b = (k,r)
                     | otherwise     = pmod_aux (k + t) (r - t)
          where t = P [((fst . highest $ r) * (inv p . fst . highest $ b), deg r - deg b)]

inv :: Integer -> Integer -> Integer
inv p a = a^(p-2) `mod` p

deg :: Poly -> Integer
deg (P []) = undefined
deg      p = snd . highest $ p

highest :: Poly -> (Integer, Integer)
highest (P []) = undefined
highest (P p)  = last . sortOn snd . normalize $ p

{-
2.37

a := x^3 + x + 1

By 2.34(a), the factors would have to have degrees 1 and 2, meaning one of them is either just a
monic polynomial of degree 1, x, or it's x+1. The first case means there can be no monomial of degree less than 1
in the final product, which means the factorization doesn't exist. In case of x+1, the second factor also has the
non-zero constant, 1. The final product would look either like x^3+x^2+... for x^2+1, or like x^3+1 for x^2+x+1,
which leads us to the conclusion that a is irreducible over F_2[x].

2.38 skipped, boring

2.39 boring, also needs code

2.40

2.41

(a) Since the field is finite, some of those sums are the same. Consider two numbers 1 <= k < n with k*1 = n*1
(where n* is just short for addition n times) that must exist since F is finite. Then n*1 - k*1 = (n-k)*1 = 0,
and n-k is obviously an integer since Z is closed under addition. QED

(b) If m is not prime, write m = kn. Then it must be true that m*1 = kn*1 = (k*1)(n*1) = 0. F is a field, so
it follows that one of k*1, n*1 is 0. But then m is not a characteristic of F, either k or n is. If neither
k*1 not n*1 are 0, it must follow that F has zero divisors, rendering it not a field (contradiction!)

(c) wat
-}
