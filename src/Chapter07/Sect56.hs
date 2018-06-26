import Lib (e)
import Lib.Matrix

-- 7.15

{-

... ¯\_(ツ)_/¯


-}

-- 7.16

{-

λ> gaussHeuristic 251 (2,2251.58)
1922.9618674339863

-}

-- detL = 2^2251.58 ~ (2,2251.58)
gaussHeuristic :: Integer -> (Double, Double) -> Double
gaussHeuristic (fromInteger -> n) detL@(b,k) = sqrt (n/(2*pi*e)) * (b**(k/n))

-- 7.17

{-

(a)
λ> babai basis717 w717
Vect {vlist = [43086.0,11448.0]}
λ> vlength $ it `vminus` w717
107.15409464878138
(b)
λ> hadamard $ rows basis717
0.9917044095752339
It is a good fairly orthogonal basis, since the Hadamard ratio is close to 1
(c)
λ> basisChange basis717 basis717c
[5.0,6.0]
[19.0,23.0]
λ> mdet it
1.0
(d)
λ> babai basis717c w717
Vect {vlist = [46548.0,9561.0]}
λ> vlength $ it `vminus` w717
3860.075776458281
(e)
λ> hadamard basis717c
3.768190815626155e-3
Apparently not, as the ratio is very small.

-}

basis717, basis717c :: Matrix Double
basis717 = matrix [[213,-437],[312,105]]
basis717c = matrix [[2937,-1555],[11223,-5888]]
w717 :: Vect Double
w717 = Vect [43127,11349]

-- take a basis and a vector w and try to solve CVP for w
babai :: (RealFrac a) => Matrix a -> Vect a -> Vect a
babai base w = vsum . combine base . fmap normalRound . vlist $ expressInBase base w
  where vsum = foldr1 vplus
        combine (rows -> b) = zipWith (flip vscal) b

-- so... apparently the Prelude's round (and, actually, the standard for floating-point
-- arithmetics) thinks it's a great idea to round 0.5 to 0 and 1.5 to 2 --- to the
-- greatest even number. Why? What is the usecase for that? The world may never know.
-- Anyway, here's a sane (away-from-zero) rounding function, tailored to our purposes.
normalRound :: RealFrac a => a -> a
normalRound x = fromIntegral $
                let (n,r) = properFraction x;
                        m = if r < 0 then n-1 else n+1
                in case signum (abs r - 0.5) of
                     -1 -> n
                     _  -> m

expressInBase :: (Fractional a, Eq a) => Matrix a -> Vect a -> Vect a
expressInBase base v = head . rows $ Matrix [v] `mmult` minverse base

basisChange :: (Fractional a, Eq a) => Matrix a -> Matrix a -> Matrix a
basisChange b b' = b' `mmult` minverse b

hadamard :: Floating a => Matrix a -> a
hadamard base = mdet base / product (map vlength $ rows base)
