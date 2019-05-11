--29 de mayo parcial
factorial :: Integer -> Integer
factorial n | n == 0 = 1
            | otherwise = n * factorial(n-1)
fib :: Integer -> Integer
fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib(n - 1) + fib(n - 2)
sc1 :: Integer -> Integer
sc1 n | n == 1 = 2
      | otherwise = 2 *(n-1)* sc1(n - 1) + (2^n)*factorial(n-1)
sc2 :: Integer -> Integer
sc2 n | n == 1 = 1
      | n == 2 = 2
      | otherwise = (n-2) * sc2(n - 1) + 2 * (n -1) * sc2(n - 2)
sc3 :: Integer -> Integer
sc3 n | n == 1 = -3
      | n == 2 = 6
      | n `mod` 2 == 0 = sc3(n-1) + 2 * sc3(n-2) + 9
      | otherwise = -sc3(n-1) -3
f1 :: Integer -> Integer
f1 n | n == 0 = 0
     | otherwise = 2^n + f1(n-1)
f2 :: (Integer, Float) -> Float
f2 (n, q) | n == 1 = q
          | otherwise = q^n + f2(n-1, q)
f3 :: (Integer, Float) -> Float
f3 (n, q) | n  == 0 = 0
          | n  == 1 = 1
          | otherwise = q^n + f2((n*2, q)
-- f3(n-1, q ) + q^2n-1	 + q^2n 
multiplode3 :: Integer -> Bool
multiplode3 n | n < 0 = False
              | n == 0 = True
              | otherwise = multiplode3( n - 3)
sumaImpares :: Integer -> Integer
sumaImpares n | n == 0 = 0
              | n `mod` 2 != 0 = 0
              | otherwise = n + sumaImpares
medioFact n | n == 0 = 1
			| n == 1 = 1
            | otherwise = n * (n-2)
