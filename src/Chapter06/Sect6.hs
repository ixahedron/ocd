import Lib.EllipticCurve hiding (dadd)

-- 6.21 --- Beware of a wild govnokod :/

{-
λ> mapM (lenstra 100) [p621a, p621b, p621c, p621d]
Just [19,191,1297,117763]
-}

-- outputs one of the factors (maybe)
lenstra :: Integer -> ELPF -> Maybe Integer
lenstra max p@(ELPF ElCurveF{..} q_) = lenstra_aux p 2
  where lenstra_aux q j | j >= max  = Nothing
                        | otherwise = case dadd q j of
                            Left z -> if z == p_ then Nothing else Just z
                            Right z -> lenstra_aux z (j+1)

-- Amended double-and-add for Lenstra's algo
dadd :: Integral a => ELPF -> a -> Either Integer ELPF
dadd p@(ELPF ElCurveF{..} _) n = dadd_aux n p O
  where dadd_aux 0 _ r = Right r
        dadd_aux n q r | odd n = checkGCDs True q r $ dadd_aux (n `div` 2) (q +^ q) (r +^ q)
                       | otherwise = checkGCDs False q r $ dadd_aux (n `div` 2) (q +^ q) r

        checkGCDs _ q O d = d
        checkGCDs True q r _ | gcd (x2-x1) p_ > 1 = Left $ gcd (x2-x1) p_
          where x1 = fst . q_ $ q; x2 = fst . q_ $ r
        checkGCDs    _ q _ d | gcd (2*y) p_ > 1 = Left $ gcd (2*y) p_
                             | otherwise = d
          where y = snd . q_ $ q

e621a = ElCurveF 4 9 589
p621a = ELPF e621a (2,5)

e621b = ElCurveF 4 128 26167
p621b = ELPF e621b (2,12)

e621c = ElCurveF 3 (-3) 1386493
p621c = ELPF e621c (1,1)

e621d = ElCurveF 18 (-453) 28102844557
p621d = ELPF e621d (7,4)

