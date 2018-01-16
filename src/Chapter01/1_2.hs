import Prelude hiding (gcd)

-- 1.6

{- (a)
   a|b <=> b = am
   b|c <=> c = bn

               Z a ring => mn in Z
   => c = amn          <=>          a|c       QED


  (b)
   a|b <=> b = ac
   b|a <=> a = bd
                          *
   => d = 1/c <=> c = 1/d => c = d = ±1      * cd = 1, c in Z, d in Z

   => a = b * ±1 <=> a = ±b                   QED



  (c)
   a|b <=> b = am
   a|c <=> c = an

   +                           def
   => b + c = am + an = a(m+n) <=> a|(b+c)

   -
   => b - c = am - an = a(m-n) <=> a|(b-c)    QED
-}

-- 1.7, 1.8 skipped because booooring

-- 1.9

gcd :: Integer -> Integer -> Integer
gcd a b = gcd' (abs a) (abs b)
  where gcd' x 0 = x
        gcd' x y = gcd' y (x `mod` y)

{-
*Main> gcd 291 252
3
*Main> gcd 16261 85652
161
*Main> gcd 139024789 93278890
1
*Main> gcd 16534528044 8332745927
43
-}

-- 1.10 (using 1.12)
uv :: Integer -> Integer -> (Integer, Integer)
uv a b = (u,v)
  where (_,u,v) = guv a b

{-
*Main> uv 291 252
(13,-15)
*Main> uv 16261 85652
(-79,15)
*Main> uv 139024789 93278890
(6944509,-10350240)
*Main> uv 16534528044 8332745927
(81440996,-161602003)
-}

-- 1.11

{-
  (a)
  
    Suppose that au + bv = 1 and gcd(a,b) /= 1.
    
    Suppose without loss of generality that a >= b.
    Then ∃q: a = bq => bqu + bv = 1 <=> b(qu+v) = 1.
    qu+v in Z. That means b = qu+v = 1 (b positive by the data).

    But if b = 1, gcd(a,b) = 1 - contradiction QED

  (b)
    a=5 b=4 u=2 v=-1
    5*2 + 4*(-1) = 6
    gcd(5,4)=1
  
  (c)
  
    au_1 + bv_1 = 1 = au_2 + bv_2 <=>
    au_2 - au_1 + bv_2 - bv_1 = 0 <=>
    a(u_2-u_1) + b(v_2-v_1) = 0   <=>
    a(u_2-u_1) = -b(v_2-v_1)      <=>
    
     in Z
    u_2-u_1 = b(v_2-v_1) / a

    gcd(a,b) = 1 => a ∤ b => a | v_2-v_1

    b | u_2-u_1 is done analogously.

  (d)

    It suffices to prove this for k=1, as we can continously
    add/subtract the appropriate values to get new solutions.

    

   
-}

-- 1.12

guv :: Integer -> Integer -> (Integer, Integer, Integer)
guv a 0 = (a,1,0)
guv a b = guv_aux 1 a 0 b
  where guv_aux u g x 0 = (g, u, (g-a*u) `div` b)
        guv_aux u g x y = guv_aux x y (u-x*(g `div` y)) $ g `mod` y

{-
*Main> guv 527 1258
(17,-31,13)
*Main> guv 228 1056
(12,-37,8)
*Main> guv 163961 167181
(7,4517,-4430)
*Main> guv 3892394 239847
(1,59789,-970295)
-}

-- (e)
guv' :: Integer -> Integer -> (Integer, Integer, Integer)
guv' a 0 = (a,1,0)
guv' a b = guv_aux 1 a 0 b
  where guv_aux u g x 0 = nnu g u ((g-a*u) `div` b)
        guv_aux u g x y = guv_aux x y (u-x*(g `div` y)) $ g `mod` y
        
        nnu g u v
            | u > 0 = (g,u,v)
            | otherwise = nnu g (u + (b `div` g)) (v - (a `div` g))

{-
*Main> guv' 527 1258
(17,43,-18)
*Main> guv' 228 1056
(12,51,-11)
*Main> guv' 163961 167181
(7,4517,-4430)
*Main>  guv' 3892394 239847
(1,59789,-970295)
-}

-- 1.13

{-
  We use the fact that gcd(a,b,c) = gcd(a, gcd(b,c)).

  Then we can rewrite the equation for three integers in the form of
    w*a + z*gcd(b,c) = gcd(a,gcd(b,c)) = gcd(a,b,c)
  and we already know this equation has its solutions w,z in Z.

  Then, by induction,
    gcd(a_1, a_2, ..., a_k) = gcd(a_1, gcd(a_2, gcd(...(gcd(a_k-1, a_k))...)
  and the hypothesis follows directly.
-}

-- 1.14

{-
  (a)
    Wanted to use q = floor(a/b) but I think that's exactly what we're trying to prove :(

    Case 1: a < 0
      Use q = a. Then b*q =< a and a - bq >= 0.

    Case 2: a >= 0
      Use q = 0. Then bq = 0 and a - bq = a >= 0.

  (b)

  (c) follows directly from (a) and (b), I think?
-}
