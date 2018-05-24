import Lib ((≡), jacobi, inv)

-- 6.1

{-

λ> pt +^ qt
ELPR {curve = ElCurveR {a = -2, b = 4}, q = (2.4444444444444455,3.703703703703707)}
λ> pt +^ pt
ELPR {curve = ElCurveR {a = -2, b = 4}, q = (0.25,-1.875)}
λ> qt +^ qt
ELPR {curve = ElCurveR {a = -2, b = 4}, q = (0.25,-1.875)}
λ> pt +^ pt +^ pt
ELPR {curve = ElCurveR {a = -2, b = 4}, q = (240.0,3718.0)}
λ> qt +^ qt +^ qt
ELPR {curve = ElCurveR {a = -2, b = 4}, q = (-1.958677685950413,-0.6348610067618328)}

-}

data ElCurveR = ElCurveR {a :: Integer, b :: Integer}
  deriving (Show, Eq)

data ELPR = OOO | ELPR {curve :: ElCurveR, q :: (Double, Double)} -- moya oborona
  deriving (Show, Eq)

class AdditiveGroup p where
  -- identity
  zero :: p
  (+^) :: p -> p -> p
  -- inverse
  negateP :: p -> p
  -- subtraction
  (-^) :: p -> p -> p
  p -^ p' = p +^ negateP p'
  -- scalar multiplication
  (*^) :: (Integral a) => a -> p -> p
  (^*) :: (Integral a) => p -> a -> p
  p ^* k = k *^ p


instance AdditiveGroup ELPR where
  zero = OOO
  negateP OOO = OOO
  negateP (ELPR c (x,y)) = ELPR c (x,-y)

  _ *^ OOO = OOO
  (*^) 0 p = OOO
  (*^) 1 p = p
  (*^) k p = p +^ ((k-1) *^ p)

  OOO +^ p2 = p2
  p1 +^ OOO = p1
  (ELPR c1 (x1,y1)) +^ (ELPR c2@ElCurveR{..} (x2,y2))
    | c1 /= c2 = error "not the same curve"
    | x1 == x2 && y1 == -y2 = OOO
    | otherwise = let λ = if (x1,y1) == (x2,y2) then (3*x1*x1+a_)/(2*y1)
                                                else (y2-y1)/(x2-x1);
                      x3 = λ^2-x1-x2; y3 = λ*(x1-x3)-y1; a_ = fromInteger a
                  in ELPR ElCurveR{..} (x3,y3)


-- 6.2

{-

λ> onRCurve p
True
λ> onRCurve q
True
λ> p +^ q
ELPR {curve = ElCurveR {a = 0, b = 17}, q = (-0.8888888888888888,-4.037037037037037)}
λ> p -^ q
ELPR {curve = ElCurveR {a = 0, b = 17}, q = (8.0,23.0)}
λ> 2 *^ p
ELPR {curve = ElCurveR {a = 0, b = 17}, q = (2.140625,-5.177734375)}
λ> 2 *^ q
ELPR {curve = ElCurveR {a = 0, b = 17}, q = (-2.56,0.4720000000000004)}

-}

onRCurve :: ELPR -> Bool
onRCurve OOO = True
onRCurve (ELPR ElCurveR{..} (x,y)) = let a' = fromInteger a; b' = fromInteger b
                                     in y^2 == x^3 + a' * x + b'


-- 6.3

{-

Y² = X³ - X²(e1+e2+e3) + X(e1e2+e2e3+e1e3) - e1e2e3
=> e1+e2+e3 = 0
A = e1e2+e2e3+e1e3
B = -e1e2e3

<=: Assume e1=e2=0. 4A³ + 27B² = 4*(0+0+0)³ + 27*0² = 0

=>: Suppose 4A³ + 27B² = 0.

For expansion, let's use x:=e1, y:=e2, z:=e3 to avoid visual clutter.

  4A³ + 27B² = 4(xy+yz+xz)³ + 27xyz² = 4x³y³ + 12x³y²z + 12x³yz² + 4x³z³ + 12x²y³z +
  + 51x²y²z² + 12x²yz³ + 12xz²y³ + 12xy²z³ + 4y³z³ = 0

Now let's factor one of the e's out and see what we can do with that:

  4A³ + 27B² = x³(4y³ + 12y²z + 12yz² + 4z³) + x²(12y³z + 51y²z² + 12yz³) +
  + x(12z²y³ + 12y²z³) + 4y³z³ = 0

We can now use the fact that for X^2 part to cancel out, we need to have x+y+z=0. So
substitute in x = 0-y-z:

  4A³ + 27B² = (-y-z)³(4y³ + 12y²z + 12yz² + 4z³) + (-y-z)²(12y³z + 51y²z² + 12yz³) +
  + (-y-z)(12z²y³ + 12y²z³) + 4y³z³ =
  = -4y⁶ - 12y⁵z + 3y⁴z² + 26y³z³ + 3y²z⁴ - 12yz⁵ - 4z⁶ = 0
  (I'll spare all the WolframAlpha suffering)

Again using WolframAlpha (I'm using it a lot ¯\_(ツ)_/¯) we find that an
"alternative form" of this expression is
  -(y-z)²(2y+z)²(y+2z)² = 0

So we see that the expression is divisible by y-z = e2-e3, so when e2=e3,
that expression is equal to zero. We could repeat the process with factoring out
e2 or e3 instead of e1 and get the analogous result. So indeed, ∆ = 0 if and only if
two or more of e's are zero.


-}

-- 6.4

{-

I used WolframAlpha. Pretty plots. Singular points appear to look like function
discontinuities if elliptic curves were functions (there's a sharp turn in the plot)


-}

-- 6.5

{-

λ> listPointsReadable (ElCurveF 3 2 7)
[O,(0,3),(0,4),(2,3),(2,4),(4,1),(4,6),(5,3),(5,4)]
λ> listPointsReadable (ElCurveF 2 7 11)
[O,(6,2),(6,9),(7,1),(7,10),(10,2),(10,9)]
λ> listPointsReadable (ElCurveF 4 5 11)
[O,(0,4),(0,7),(3,0),(6,5),(6,6),(9,0),(10,0)]
λ> listPointsReadable (ElCurveF 9 5 11)
[O,(0,4),(0,7),(1,2),(1,9),(2,3),(2,8),(3,2),(3,9),(6,0),(7,2),(7,9),(9,1),(9,10)]
λ> listPointsReadable (ElCurveF 9 5 13)
[O,(4,1),(4,12),(8,2),(8,11),(9,3),(9,10),(10,4),(10,9)]

-}


data ElCurveF = ElCurveF {a :: Integer, b :: Integer, p :: Integer}
  deriving (Show, Eq)

data ELPF = O | ELPF {curve :: ElCurveF, q :: (Integer, Integer)}
  deriving (Show, Eq)

onCurve :: ELPF -> Bool
onCurve O = True
onCurve (ELPF ElCurveF{..} (x,y)) = y^2 ≡ (x^3 + a*x + b) $ p

listPoints :: ElCurveF -> [ELPF]
listPoints e = O : map (ELPF e) (listPointsReadable e)

listPointsReadable :: ElCurveF -> [(Integer,Integer)]
listPointsReadable e@ElCurveF{..} = concatMap points [0..p-1]
  where points x | jacobi (x^3 + a*x + b) p == -1 = []
                 | otherwise = [(x,y) | y<-[0..p-1], onCurve $ ELPF e (x,y)]


-- 6.6

{-

I don't fancy doing that, but I wrote code for addition, so maybe that's enough? ¯\_(ツ)_/¯

-}

instance AdditiveGroup ELPF where
  zero = O
  negateP O = O
  negateP (ELPF c@ElCurveF{..} (x,y)) = ELPF c (x,p-y)

  (*^) _ O = O
  (*^) 0 p = O
  (*^) 1 p = p
  (*^) k p = p +^ ((k-1) *^ p)

  O +^ p2 = p2
  p1 +^ O = p1
  p1 +^ p2 | (not . onCurve $ p1) || (not . onCurve $ p2) = error "points not on the curve"
           | p1 == negateP p2 = O
           | negateP p1 == p2 = O
  (ELPF c1 (x1,y1)) +^ (ELPF c2@ElCurveF{..} (x2,y2))
    | c1 /= c2 = error "not the same curve"
    | otherwise = let λ = if (x1,y1) == (x2,y2) then (3*x1*x1+a) * inv (2*y1) p
                                                else (y2-y1) * inv (x2-x1) p;
                      x3 = (λ^2-x1-x2) `mod` p; y3 = (λ*(x1-x3)-y1) `mod` p;
                  in ELPF ElCurveF{..} (x3,y3)


-- 6.7

{-

λ> map (\p -> let np = length_ (listPoints $ e p); p' = fromInteger p in (np, p+1-np, 2*sqrt p')) [3,5,7,11]
[(4,0,3.4641016151377544),(9,-3,4.47213595499958),(5,3,5.291502622129181),(14,-2,6.6332495807108)]

-}
