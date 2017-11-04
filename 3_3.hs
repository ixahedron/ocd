import Lib

-- 3.12

{-

Both of these are similar and involve faking public keys, obviously.
If Bob has no way to make sure the public key belongs to Alice and
not Eve, they're both screwed and Eve wins.

-}

-- 3.13

{-

Prelude> gcd 1021763679 519424709
1

So we're good to go

*Main> let (u,v) = euc e1 e2              
*Main> ((nexp c1 u) * (nexp c2 v)) `mod` n
1054592380

*Main> let m = it
*Main> nexp m e1
1244183534
*Main> nexp m e2
732959706

Checks out, good

-}

-- wow, that was a quickly done homework
