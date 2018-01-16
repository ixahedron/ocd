import Data.Numbers.Primes (primes, primeFactors)
import Data.List(group, sort)

-- 2.26

{-

By Remark 1.32, if k divides p−1, then there are exactly φ(k) elements
of F_p* having order k. Considering that φ(k) > 0 for every k > 2,
our work here is done.

-}

-- 2.27 skipped

-- 2.28

{-

*Main> pha 433 7 166
Just 47
*Main> pha 746497 10 243278
Just 223755
*Main> pha 41022299 2 39183497
Just 33703314
*Main> pha 1291799 17  192988
Just 984414

-}


--pha :: Integer -> Integer -> Integer -> Maybe Integer
pha p g h = crt ys
  where gs = map (\x -> g^(n `div` x) `mod` p) ppowers 
        hs = map (\x -> h^(n `div` x) `mod` p) ppowers
        ys = map (\(g, h, qe) -> (solveSmallDLP g h p qe, qe)) (zip3 gs hs ppowers)

        n = lorder g p
        ppowers = primePowers n


solveSmallDLP :: Integer -> Integer -> Integer -> Integer -> Integer
solveSmallDLP g h p o = sbg p o g h


order :: Integer -> Integer -> Integer
order g p = head [x | x <- factors (p-1), g^x `mod` p == 1]

lorder :: Integer -> Integer -> Integer
lorder g p = head [x | x <- [1..p-1], g^x `mod` p == 1]

factors n = ds ++ [r | mod n r == 0] ++ reverse (map (n `div`) ds)
        where
        r = floor (sqrt (fromIntegral n))
        ds = [i | i <- [1..r-1], mod n i == 0]

primePowers :: Integer -> [Integer]
primePowers = map product . group . primeFactors


crt :: [(Integer, Integer)] -> Maybe Integer
crt [] = Nothing
crt ((f,s):xs) = crt' xs (f,s)
  where crt' []         (r,m) = Just r
        crt' ((t,n):xs) (r,m) = if gcd m n == 1 then crt' xs (sol, m*n) else Nothing -- not coprime
          where sol = r + invy * m 
                invy = (inv m n * (norm t-r)) `mod` n

                norm x = if x >= 0 then x else x+n


inv :: Integer -> Integer -> Integer -- multiplicative inverse using EEA
inv a p = norm . fst $ euc a p
  where norm x = if x >= 0 then x else x+p

-- euc a b = (x,y) => ax + by = (gcd a b)^2
euc :: Integer -> Integer -> (Integer, Integer)
euc a b = (g*x,g*y)
  where g = gcd a b
        (x,y) = euc' (a `div` g) (b `div` g)
        euc' a b = case b of
                 1 -> (0, 1)
                 _ -> let (e, f) = euc' b d
                    in (f, e - c*f)
          where c = a `div` b
                d = a `mod` b



sbg :: Integer -> Integer -> Integer -> Integer -> Integer
sbg p order g h = i + j*n
  where n = 1 + floor (sqrt $ fromIntegral order)
        lg = sort [(g^k `mod` p, k) | k<-[0..n]]
        lh = sort [(h * (ing^k) `mod` p, k) | k<-[0..n]]
        (i,j) = fm (0,0) $ match lg lh
        
        ing = inv (g^n) p

        fm _ (Just x) = x
        fm d       _  = d

match :: [(Integer, Integer)] -> [(Integer, Integer)] -> Maybe (Integer, Integer)
match [] _ = Nothing
match _ [] = Nothing
match xx@((x,i):xs) yy@((y,j):ys) | x == y = Just (i,j)
                                  | x > y = match xx ys
                                  | otherwise = match xs yy


