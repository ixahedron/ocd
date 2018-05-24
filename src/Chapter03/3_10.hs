import Lib (mexp, jacobi)
import Data.Numbers.Primes (primeFactors)

-- 3.42

{-

(a)
λ> map (decrypt prv342a) [1794677960, 525734818, 420526487]
[True,False,True]

(b)
λ> map (decrypt prv342b) [2322, 719, 202]
[True,False,False]

(c) map (\(bit, r) -> encrypt pub342c bit r) $ zip [True, True, False] [705130839, 631364468, 67651321]
[517254876,4308279,660699010]

-}

prv342a = PrvK 32411 56843
prv342b = factorPubK pub342b

pub342b = PubK 3149 2013
pub342c = PubK 781044643 568980706

data PubK = PubK { pubN :: Integer
                 , pubA :: Integer
                 } deriving Show
data PrvK = PrvK { prvP :: Integer
                 , prvQ :: Integer
                 } deriving Show


encrypt :: PubK -> Bool -> Integer -> Integer
encrypt (PubK n a) bit r = let q = if bit then a else 1
                             in (q * mexp n r 2) `mod` n

decrypt :: PrvK -> Integer -> Bool
decrypt prvk c = case jacobi c $ prvP prvk of
                   1 -> False
                   -1 -> True
                   _ -> error "incorrect jacobi"

factorPubK :: PubK -> PrvK
factorPubK pubk = let [p,q] = primeFactors $ pubN pubk
                  in PrvK p q

-- 3.43

{-

(a) d_k c = m = r || (r ⊕ m'). We know that ⊕ (xor)
is invertible, so compute
  m' = r ⊕ (r ⊕ m') = (r ⊕ r) ⊕ m' = 0 ⊕ m' = m'

(b) 2b/b = 2

(c) 2μb / b = 2μ

-}
