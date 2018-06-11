module Lib.AddGroup where

class AddGroup p where
  -- identity
  zero :: p
  (+^) :: p -> p -> p
  (^+) :: p -> p -> p
  (^+) = (+^)
  -- inverse
  negateP :: p -> p
  -- subtraction
  (-^) :: p -> p -> p
  p -^ p' = p +^ negateP p'
  -- scalar multiplication
  (*^) :: (Integral a) => a -> p -> p
  (^*) :: (Integral a) => p -> a -> p
  p ^* k = k *^ p
