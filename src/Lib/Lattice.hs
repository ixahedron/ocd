module Lib.Lattice ( Basis
                   , detL
                   , hadamard
                   , babai
                   , expressInBase
                   , basisChange
                   , af0Round
                   ) where

import Lib.Matrix


----------------------------------------------------------

type Basis a = Matrix a

detL :: Num a => Basis a -> a
detL base = abs $ mdet base

hadamard :: Floating a => Basis a -> a
hadamard base = let n = fromIntegral (let (n',_) = mdim base in n')
                in (detL base / (product . map vlength . rows $ base))**(1/n)


-- take a basis and a vector w and try to solve CVP for w
babai :: (RealFrac a) => Basis a -> Vect a -> Vect a
babai base w = vsum . combine base . vlist . fmap af0Round $ expressInBase base w
  where vsum = foldr1 vplus
        combine (rows -> b) = zipWith (flip vscal) b

-- so... apparently the Prelude's round (and, actually, the standard for floating-point
-- arithmetics) thinks it's a great idea to round 0.5 to 0 and 1.5 to 2 --- to the
-- greatest even number. Why? What is the usecase for that? The world may never know.
-- Anyway, here's a sane (away-from-zero) rounding function, tailored to our purposes.
af0Round :: RealFrac a => a -> a
af0Round x = fromIntegral $
              let (n,r) = properFraction x;
                      m = if r < 0 then n-1 else n+1
              in case signum (abs r - 0.5) of
                   -1 -> n
                   _  -> m


expressInBase :: (Fractional a, Eq a) => Basis a -> Vect a -> Vect a
expressInBase base v = head . rows . basisChange base $ Matrix [v]


basisChange :: (Fractional a, Eq a) => Basis a -> Basis a -> Basis a
basisChange b b' = b' `mmult` minverse b

