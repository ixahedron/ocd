module Lib.Field where

type Fp2Elem = (Integer,Integer)

instance Num Fp2Elem where
  negate (x,i) = (-x,-i)
  (x,i)+(w,j)  = (x+w,i+j)
  (x,i)*(w,j)  = (x*w-i*j,x*j+w*i)
  fromInteger  = (,0)
  abs (x,i)    = (abs x, abs i) -- this is probably wrong, fix?
  signum (x,i) = (signum x, signum i)

class (Eq p, Num p) => Moddable p where
  pmod :: Integral n => p -> n -> p
instance Moddable Fp2Elem where
  pmod (x,i) (fromIntegral -> p) = (x `mod` p, i `mod` p)
instance Moddable Integer where
  pmod a (fromIntegral -> p) = mod a p

class (Eq p, Num p, Moddable p) => Fieldish p where
  pinv :: Integral n => p -> n -> p
  zero :: p
  zero = fromInteger 0

instance Fieldish Integer where
  pinv (fromInteger -> a) (fromIntegral -> p) = inv a p

instance Fieldish Fp2Elem where
  pinv (x,i) (fromIntegral -> p) = ((x,-i)*(fromInteger $ inv (x*x+i*i) p)) `pmod` p

infix 5 ≡
(≡) :: (Moddable a, Integral n) => a -> a -> n -> Bool
a ≡ b = \p -> (a `pmod` p) == (b `pmod` p)

-- multiplicative inverse using EEA
inv :: (Integral n, Show n) => n -> n -> n
inv a p = if gcd a p == 1
          then (fst $ euc a p) `mod` p
          else error $ show p ++ " can't compute inverse if gcd =/= 1 " ++ show a

-- modular (fast) exponentiation using binary method
-- mexp m a e = a^e (mod m)
mexp :: (Integral n, Fieldish a) => n -> a -> n -> a
mexp 1 _ _ = 0
mexp m x y = raise x y
  where raise _    0 = fromInteger 1
        raise a    1 = a `pmod` m
        raise a e | a == zero = zero
                  | e < 0  = raise (pinv a m) (-e)
                  | otherwise = let t = if odd e then a `pmod` m else fromInteger 1
                    in (t * (raise ((a*a) `pmod` m) (e `div` 2))) `pmod` m

-- euc a b = (x,y) => ax + by = gcd a b
euc :: Integral n => n -> n -> (n,n)
euc a b | a `mod` b == 0 = (0,1)
        | otherwise = (y,x-y*(a `div` b))
  where (x,y) = euc b $ a `mod` b

-- same as euc but returning (x,y,gcd a b)
eGCD :: Integral n => n -> n -> (n,n,n)
eGCD a b | mod a b == 0 = (0,1,b)
         | otherwise = (y,x-y*(a `div` b),z)
        where
          (x,y,z) = eGCD b $ a `mod` b
