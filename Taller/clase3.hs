sumaUnidades3 :: Integer -> Integer -> Integer -> Integer
sumaUnidades3 x y z = x + y + z
esPar :: Integer -> Bool
esPar x = mod x 2 == 0
todosImpares :: Integer -> Integer -> Integer -> Bool
todosImpares x y z =  not(esPar x && esPar y && esPar z)
alMenosUnImpar :: Integer -> Integer -> Integer -> Bool
alMenosUnImpar x y z =  not(esPar x) || not(esPar y) || not(esPar z)
esParBinario :: Integer -> Integer
esParBinario x | esPar x = 1
               | otherwise = 0
conteoPares :: Integer -> Integer -> Integer -> Integer
conteoPares x y z = esParBinario x + esParBinario y + esParBinario z
alMenosDosImpares :: Integer -> Integer -> Integer -> Bool
alMenosDosImpares x y z =  conteoPares x y z <=1
alMenosDosPares :: Integer -> Integer -> Integer -> Bool
alMenosDosPares x y z = conteoPares x y z >= 2
alMenosUnMultiploDe :: Integer -> Integer -> Integer -> Bool
alMenosUnMultiploDe x y z = mod z x == 0 || mod z y == 0
mul :: Integer -> Integer -> Bool
mul x y | y/= 0 = x `mod` y == 0
inv :: Float -> Float
inv x | x /= 0 = 1/x
--a ∼ b si a y b tienen la misma paridad
r1 :: Integer -> Integer -> Bool
r1 a b = (esPar a && esPar b) || (not(esPar a) && not(esPar b))
--a ∼ b si 2a + 3b es divisible por 5
r2 :: Integer -> Integer -> Bool
r2 a b = mul (2*a + 3*b) 5
--(a, b), (p, q) ∈ R6=0 × R6=0, (a, b) ∼ (p, q) si ∃k ∈ R tal que (a, b) = k(p, q)
rel1 :: (Float,Float) -> (Float,Float) -> Bool
rel1 (a,b) (p,q) = a/p == b/q
--(a, b), (p, q) ∈ Z6=0 × Z6=0, (a, b) ∼ (p, q) si existe k ∈ R tal que (a, b) = k(p, q)
rel2 :: (Integer,Integer) -> (Integer,Integer) -> Bool
rel2 (a,b) (p,q) = a `div` p == b `div` q
--(a, b), (p, q) ∈ Z × Z − {(0, 0)} (a, b) ∼ (p, q) si existe k ∈ R tal que (a, b) = k(p, q)
rel3 :: (Integer,Integer) -> (Integer,Integer) -> Bool
rel3 (a,b) (p,q)| a == 0 && b == 0 || p == 0 && q == 0 = False
                | otherwise = rel2 (a,b) (p,q)
factorial :: Integer -> Integer
factorial n
    | n > 0 = n * factorial (n-1)
    | n == 0 = 1
sc :: Integer -> Integer
sc n | n == 0 = 0
     | otherwise = sc(n - 1) + n
fib :: Integer -> Integer
fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib(n - 1) + fib(n - 2)
--a1 = 2, an+1 = 2nan + 2n+1n!, para todo n ∈ N
sc1 :: Integer -> Integer
sc1 n | n == 1 = 2
      | otherwise = 2*n*sc1(n - 1) + (2^(n + 1))*factorial(n)
--a1 = 1, a2 = 2 y an+2 = nan+1 + 2(n + 1)an, para todo n ∈ N.
sc2 :: Integer -> Integer
sc2 n | n <= 2 = n
      | otherwise = n * sc2(n - 1) + 2*(n + 1) * sc2(n - 2)


      | otherwise = 2*n*sc1(n-1) + (2^(n+1))*factorial(n)