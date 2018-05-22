import Lib.Field (inv)

-------------------------------------
----------- Section 7.1 -------------
-------------------------------------

-- 7.1

{-

(a) λ> pubK 918293817 (19928, 18643)
(918293817,767748560)
(b) λ> enc (19928, 18643) (918293817,767748560) 619168806
11818
(c) λ> enc (918293817,767748560) 10220 19564
619167208

-}

type PrivK = (Integer,Integer)
type PubK  = (Integer,Integer)

pubK :: Integer -> PrivK -> PubK
pubK q (f,g) = (q,(g * inv f q) `mod` q)

enc :: PubK -> Integer -> Integer -> Integer
enc (q,h) m r = (r*h + m) `mod` q

dec :: PrivK -> PubK -> Integer -> Integer
dec (f,g) (q,h) e = let a = (f*e) `mod` q;
                        b = (a * inv f g) `mod` g
                    in b

-------------------------------------
----------- Section 7.2 -------------
-------------------------------------

-- 7.2

{-

λ> solveSubsetWithNumbers [3, 7, 19, 43, 89, 195] 260
Right [3,19,43,195]
λ> solveSubsetWithNumbers [5, 11, 25, 61, 125, 261] 408
Left "solution non-existent"
λ> solveSubsetWithNumbers [2, 5, 12, 28, 60, 131, 257] 334
Right [5,12,60,257]
λ> solveSubsetWithNumbers [4, 12, 15, 36, 75, 162] 214
Left "sequence non-superincreasing"

-}

solveSubset :: [Integer] -> Integer -> Either String [Integer]
solveSubset ms s = solve_aux (reverse ms) s []
  where solve_aux [] 0 xs | sum (zipWith (*) xs ms) == s = Right xs
                          | otherwise = Left "something exploded :("
        solve_aux [] _ _  | not . isSuperincreasing $ ms = Left "sequence non-superincreasing"
                          | otherwise = Left "solution non-existent"
        solve_aux (mi:mis) s' xs | s' >= mi = solve_aux mis (s'-mi) (1:xs)
                                 | otherwise = solve_aux mis s' (0:xs)

solveSubsetWithNumbers :: [Integer] -> Integer -> Either String [Integer]
solveSubsetWithNumbers ms s = case solveSubset ms s of
                                Left e -> Left e
                                Right is -> Right $ zipWith (*) is ms

isSuperincreasing :: [Integer] -> Bool
isSuperincreasing  [] = True
isSuperincreasing [x] = True
isSuperincreasing (i:j:xs) = (j >= 2*i) && isSuperincreasing (j:xs)

-- 7.3

{-

To find r, we should clearly multiply M by something. From M_i = A*r_i (mod B)
we conclude that r_i ≡ A^-1 * M_i (mod B) and so r_i = A^-1 * M_i + B*k for some k in Z.
It's clear that we can take k = 0 for the smallest possible result. This
result will be correct because B > 2*r_n.

λ> findSequence m (4392,8387)
[5,14,30,75,160,351,750,1579,3253,6510]

λ> crackK m pk 4398
[0,1,1,0,0,1,1,0,1,0]

-}

crackK :: KPubK -> PrivK -> Integer -> [Integer]
crackK ms pk = let r = findSequence ms pk in decK r pk

decK :: [Integer] -> PrivK -> Integer -> [Integer]
decK r (a,b) s = case solveSubset r s of
                   Left e -> error e
                   Right x -> x

type KPubK = [Integer]

findSequence :: KPubK -> PrivK -> [Integer]
findSequence ms (a,b) = let t = inv a b in map ((`mod` b) . (*t)) ms

-- 7.4 nu bļeģ..........


-------------------------------------
----------- Section 7.3 -------------
-------------------------------------

-- 7.5

{-

(a)
If B and B' are the givem basises in matrix form, then to make a change from B' to B,
we need C := BD with D := {B'}^-1, so that we can multiply and get CB' = BDB' = B{B'}^-1 B' = B

And, I mean, I should probably write code for matrix multiplication and inversion, but I
can't be bothered right now, sorry. Maybe later, although that always seems to mean never :/
So here are some WolframAlpha induced answers:

D = {B'}^-1 = (-1/3 | 0 | 2/3
                4/3 | 1 | -5/3
                1/3 | 0 | 1/3)

C = BD = (13/3 | 3  | -11/3
            -1 | -1 | 4
           1/3 | 0  | 4/3)

(b) λ> vlength v
3.7416573867739413
λ> vlength w
4.58257569495584
λ> dot v w
8
λ> vangle v w
1.0853880929848332 (radians)

-}

type Vector = [Double]

dot :: Vector -> Vector -> Double
dot v w = sum $ zipWith (*) v w

vlength :: Vector -> Double
vlength v = sqrt $ dot v v

vangle :: Vector -> Vector -> Double
vangle v w = let θ = vw / (vv * ww);
                 vw = dot v w;
                 vv = vlength v;
                 ww = vlength w
             in acos θ


-- 7.6

{-

λ> gram vs1
[[1.0,3.0,2.0],[3.7857142857142856,0.3571428571428572,-2.4285714285714284],[0.19649122807017516,-0.24561403508771917,0.2701754385964916]]
λ> gram vs2
[[4.0,1.0,3.0,-1.0],[2.5925925925925926,1.1481481481481481,-2.5555555555555554,3.851851851851852],[-0.7229219143576826,-1.0201511335012596,2.0125944584382864,2.1259445843828715]]

-}

gram :: [Vector] -> [Vector]
gram [] = []
gram (v:vs) = gram_aux [v] vs
  where gram_aux ovs [] = ovs
        gram_aux ovs (vi:vis) = let vi' = zipWith (-) vi vjsum
                                    vjsum = foldr1 (zipWith (+)) (map δ ovs)
                                    δ vj = map (* μ vj) vj
                                    μ vj = (dot vi vj) / (dot vj vj)
                                in gram_aux (ovs ++ [vi']) vis

