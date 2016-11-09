import Data.List

-- 2.8

{-
*Main> pub_2_8 947
177
*Main> encrypt_2_8b 
(719,618)
*Main> decrypt_2_8c
332
*Main> decrypt_2_8d
Just 365
-}

pub :: Integer -> Integer -> Integer -> Integer
pub p g priv = g^priv `mod` p

pub_2_8 = pub 1373 2

encrypt :: Integer -> Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
encrypt p g a k m = (c1, c2)
    where c1 = g^k `mod` p
          c2 = m * (a^k) `mod` p

encrypt_2_8b = encrypt 1373 2 (pub_2_8 947) 877 583

inv p a = a^(p-2) `mod` p

decrypt :: Integer -> Integer -> (Integer, Integer) -> Integer
decrypt p priv (c1, c2) = (inv p $ c1^priv) * c2 `mod` p

decrypt_2_8c = decrypt 1373 299 (661, 1325)

dlog :: Integer -> Integer -> Integer -> Maybe Integer
dlog p g a = dlog' 0
  where dlog' n
            | n > p-1 = Nothing
            | g^n `mod` p == a = Just n
            | otherwise = dlog' $ n+1

crack :: Maybe Integer -> Maybe Integer
crack Nothing = Nothing
crack (Just b) = Just $ decrypt 1373 b (693, 793)

decrypt_2_8d = crack (dlog 1373 2 893)

-- 2.9

{-

A = g^a is public. If Eve intercepts the ciphertext (c1, c2) = (g^k, mA^k), then she can run
her oracle with A and c1 and get g^ak = A^k. She already has c2 = mA^k. She can then find
an inverse for A^k and get m by computing

  m * A^k * (A^k)^-1 = m

-}

-- 2.11

{-
(a) τσ² = τσ * σ = σ²τ * σ = σ² * σ²τ = σ³ * στ = e * στ = στ
(b) τ(στ) = (τσ)τ = (σ²τ)τ = (σ²)ττ = σ²
(c) (στ)(στ) = σ(τσ)τ = σ(σ²τ)τ = σ³ττ = e*e = e
(d) (στ)(σ²τ) = (στ)(τσ) = σ(ττ)σ = σ²

S3 is not commutative, because otherwise it would be true that

  στ = τσ² = σ²τ

but those two are two distinct elements.

-}

-- 2.12

{-

(a) If g^d = e, then

  g^d = e = e^-1 = (g^d)^-1 = (g^-1)^d

Hence, g^-1 is in G[d].

(b) g1^d * g2^d = e * e = e, but also, by expanding the powering,
it holds that             d times            d times    comm.
  g1^d * g2^d = g1 * g1 * ... * g1 * g2 * g2 * ... * g2   = 
                    d times
  g1 * g2 * g1 * g2 * ... * g1 * g2 = (g1 * g2)^d = e,

so the product of g1 and g2 is in G[d]

(c) G[d] is a subgroup by definition, proof of (a) and (b), and the fact that e^d = e.
Subgroup itself is a group. QED

(d) Let d = 2 and G = S3 from 2.11. Then,
     2.11c   
  στ² = e and τ² = e, but
         2.11b
  (τστ)²  =  (σ²)² = σ³σ = σ ≠ e

-}

-- 2.13

{-

(a) φ(g * eG) = φ(g) = φ(g) * eH
                                 == eH
    φ(g * g^-1) = φ(g) * φ(g^-1)   =   φ(g) * φ(g)^-1 => φ(g^-1) = φ(g)^-1

(b) φ(g1 * g2) = (g1 * g2)^2 = g1^2 * g2^2 = φ(g1) * φ(g2)
Use S3 from 2.11 as a counterexample.

(c)

-}

-- 2.17

{- Shit's not working yet

flrt :: Integer -> Integer  -- flrt x ≈ √x,  with  flrt x^2 ≤ x < flrt(x+1)^2
flrt x = approx (round . (sqrt::Double->Double) . fromInteger $ x)
   where approx r
            | ctrl <= x, (r+1)^2 > x  = r
            | otherwise               = approx $ r - diff
          where ctrl = r^2
                diff = (ctrl - x) // (2*r)    -- ∂/∂x x² = 2x

         a//b = a`div`b + if (a>0)==(b>0) then 1 else 0   -- always away from 0

sbg :: Integer -> Integer -> Integer -> Integer -> Integer
sbg p order g h = i + j*n
  where n = 1 + flrt order
        lg = sort [(g^k, k) | k<-[0..n]]
        lh = sort [(h * inv p g^(n*k), k) | k<-[0..n]]
        (i,j) = match lg lh

match :: [(Integer, Integer)] -> [(Integer, Integer)] -> Maybe (Integer, Integer)
match [] _ = Nothing
match _ [] = Nothing
match xx@((x,i):xs) yy@((y,j):ys) | x == y = Just (i,j)
                                  | x > y = match xx ys
                                  | otherwise = match xs yy

-}                                  
