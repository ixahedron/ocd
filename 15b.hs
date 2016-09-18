der :: Integer -> Integer
der 0 = 1
der 1 = 0
der n = (n-1) * ((der $ n-1) + (der $ n-2))
