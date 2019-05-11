funcion3 :: Integer -> Integer -> Bool -> Bool
funcion3 x y b = b || (x > y)
doble :: Integer -> Integer
doble x = x + x
cuadruple :: Integer -> Integer
cuadruple x = doble (doble x)
esPar :: Integer -> Bool
esPar x = mod x 2 == 0
esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y = mod x y == 0
triple :: Num a => a -> a
triple x = 3 * x
normaVectorial :: (Float, Float) -> Float
normaVectorial p = sqrt ((fst p) ^ 2 + (snd p) ^ 2)
crearPar :: a -> b -> (a,b)
crearPar x y = (x,y)
invertir :: (a,b) -> (b,a)
invertir (x,y) = (y,x)
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x,y) (a,b) = normaVectorial (x-a, y-b)