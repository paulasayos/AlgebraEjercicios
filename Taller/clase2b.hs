triple :: Num a => a -> a
triple x = x * 3
normaVectorial :: (Float, Float) -> Float
normaVectorial p = sqrt ((fst p) ^ 2 + (snd p) ^ 2)
crearPar :: a -> b -> (a,b)
crearPar x y = (x,y)
esPar :: Integer -> Bool
esPar x = mod x 2 == 0
invertir :: (a,b) -> (b,a)
invertir (x,y) = (y,x) 
f1 :: Float -> (Float, Float, Float)
f1 x = (2*x, x^2,x-7)
f2 :: Integer -> Integer
f2 x | esPar x = div x 2
   | otherwise = (x + 1)
f :: Integer -> Integer
f x | mod x 6 == 0 = div (x^2) 2
   | otherwise = (3*x) + 1
g :: (Integer, Integer) -> Integer
g x = fst x * (snd x + 1)
h :: (Integer, Integer) -> Integer
h x =  f(g x)
