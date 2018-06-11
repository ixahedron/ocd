import Data.Ratio

import Lib.Matrix

-------------------------------------
----------- Section 7.4 -------------
-------------------------------------

-- 7.7 skip drawing

{-

VolF = |detA| = |det( (1,3,-2), (2,1,0), (-1,2,5) )| = |-35| = 35

-}

-- 7.8

{-

If we have that property for a single vector 0, we can use the fact that L is
a subgroup (closed under addition) and "move" the ϵ-neighborhood to any other
point v∈L, which will then only contain the point v. Then it's obvious that L
is discrete.

-}

-- 7.9

{-

Lattice <=> discrete additive subgroup

<=: already shown in 7.8, also obvious from the definition of lattices

=>: I think it's more or less clear that a lattice should be an additive subgroup,
since otherwise it's not closed under addition, and it needs to be by definition,
otherwise some a_i would not be permitted. The discreteness is needed to achieve
zero coefficients only in the linear combinations forming the lattice, since otherwise
there must exist two points whose difference is less than any vector already in the
lattice, which is a contradiction.

-}

-- 7.10

{-

(a) ((9

(b) If detA = 0, the matrix is non-invertible. If it's not, we use (a):

  1/det(A) * B = 1/(detA) * BAA^-1 = 1/det(A)*det(A) * In * A^-1 = A^-1

(c) Integer inverse exists <=> det = ±1

=>: detA must be an integer. Compute

  det(A)(A^-1) = det(AA^-1) = det(In) = 1

So the only viable solution is if det(A) = ±1.

<=: Using (b):

  A^-1 = 1/det(A) * B = ±B

which obviously only has integer entries.

(d) Unit _is_ invertible by definition. Not sure if it automatically follows entries are in R.

-}

-- 7.11

{-

(a) ±1 * ±1 = detA * detB = det(AB) => AB in GLn(ℤ) by 7.10(c)
(b) 1 = det(I) = det(AA^-1) = det(A)det(A^-1) = ±1 * det(A^-1) => det(A^-1) = detA = ±1
    => A^-1 in GLn(ℤ) by 7.10(c)
(c) det(I) = 1
(d) Uhhuh, it's true for any matrices in ℝ.
(e) ...No?

-}

-- 7.12

{-

λ> mdet $ Matrix [Vect [3,1], Vect [2,2]]
4
λ> mdet $ Matrix [Vect [3,-2], Vect [2,-1]]
1
λ> mdet $ Matrix [Vect [3,2,2], Vect [2,1,2], Vect [-1,3,1]]
-9
λ> mdet $ Matrix [Vect [-3,-1,2], Vect [1,-3,-1], Vect [3,0,-2]]
1

So (b) and (d) are in GLn(ℤ).

λ> mGLnInverse $ Matrix [Vect [3,-2], Vect [2,-1]]
[-1,2]
[-2,3]

λ> mGLnInverse $ Matrix [Vect [-3,-1,2], Vect [1,-3,-1], Vect [3,0,-2]]
[6,-2,7]
[-1,0,-1]
[9,-3,10]

-}

mGLnInverse :: Integral n => Matrix n -> Matrix n
mGLnInverse m | mdet m `notElem` [1,-1] = error "matrix not in GLn(ℤ)"
              | otherwise = mdet m `mscal` madjoint m


-- 7.13

{-

λ> mdet basis713b
-48
λ> mdet basis713b1
-48
λ> mdet basis713b2
96

So B1 is also a basis, but B2 isn't.

To make a change from B to B', we need C := B'D with D := {B}^-1, so that we can multiply
out and get CB = B'DB = B'{'}^-1 B = B'.

λ> basisChange basis713b basis713b1
[0,-3,2]
[1,1,-1]
[-2,3,-1]

-}

basisChange :: (Eq a, Fractional a) => Matrix a -> Matrix a -> Matrix a
basisChange b b' = mmult b' (minverse b)

basis713b, basis713b1, basis713b2 :: Matrix Rational
basis713b  = Matrix [Vect [3,1,-2], Vect [1,-3,5], Vect [4,2,1]]
basis713b1 = Matrix [Vect [5, 13, -13], Vect [0,-4,2], Vect [-7,-13,18]]
basis713b2 = Matrix [Vect [4,-2,3], Vect [6,6,-6], Vect [-2,-4,7]]

-- 7.14

{-

(a)
(b) Using (a), note that Gram(...) = F*F^T. Also remember that transposing a
matrix does not change its determinant.

  det(Gram) = det(F*F^T) = det(F)det(F^T) = det(L)'*det(L)' = det(L)'^2 = det(L)^2

What I denote by det(L)' is the entity that needs to be taken by absolute value to
get actual det(L). Of course, by squaring we get rid of this particular problem.

(c)
λ> gram basis714
[3,-3,2]
[-3,21,3]
[2,3,7]
λ> sqrt . mdet $ gram basis714
15.198684

(d) Trivial by Proposition 7.19 and the relation ||v||^2 = (v `dot` v)
-}

gram :: Num a => Matrix a -> Matrix a
gram basis = let (m,_) = mdim basis in Matrix $
      map (\i -> Vect $ map (\j -> mi basis i `dot` mi basis j) [1..m]) [1..m]

basis714 :: Matrix Float
basis714 = Matrix [Vect [1,0,1,-1], Vect [1,2,0,4], Vect [1,-1,2,1]]
