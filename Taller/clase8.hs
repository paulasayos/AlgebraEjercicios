pertenece :: Integer -> [Integer] -> Bool
pertenece n xs | length xs == 0 = False
               | (head xs) == n = True
               | otherwise = pertenece n (tail xs)
listar :: a -> a -> a -> [a]
listar x y z = [x,y,z] 
listar2 :: a -> a -> a -> [a]
listar2 x y z = x:y:z:[]
primerMultiplode45345 :: [Integer] -> Integer 
primerMultiplode45345 xs | (mod 45345 (head xs) ) == 0 = head xs
                         | otherwise = primerMultiplode45345 (tail xs)
productoria :: [Integer] -> Integer --que devuelve la productoria de los elementos.
productoria [] = 1
productoria (x:xs) = x * productoria xs 
sumarN :: Integer -> [Integer] -> [Integer] -- que dado un numero N y una lista xs, suma N a cada elemento de xs.
sumarN n [] = []
sumarN n (x:xs) = (x+n): (sumarN n xs)
sumarElPrimero :: [Integer] -> [Integer] -- que dada una lista no vac´ıa xs, suma el primer elemento a cada elemento de xs. Ejemplo sumarElPrimero [1,2,3] [2,3,4]
sumarElPrimero (x:xs) = (x+x):sumarN x xs
sumarElUltimo :: [Integer] -> [Integer] --que dada una lista no vac´ıa xs, suma el ultimo elemento a cada elemento de xs. Ejemplo sumarElUltimo [1,2,3] [4,5,6]
sumarElUltimo (x:xs) = (x+(ultimoLista xs)):(sumarN (ultimoLista xs) xs)
ultimoLista :: [Integer] -> Integer
ultimoLista xs | length xs == 1 = head xs
               | otherwise = ultimoLista (tail xs)
pares :: [Integer] -> [Integer] -- que devuelve una lista con los elementos pares de la lista original. Ejemplo pares [1,2,3,5,8] [2,8]
pares (x:xs) | length xs == 0 = []
             | mod x 2 == 0 = x:pares xs
             | otherwise = pares xs
multiplosDeN :: Integer -> [Integer] -> [Integer] -- que dado un n´umero N y una lista xs, devuelve una lista con los elementos multiplos N de xs.
multiplosDeN n xs | length xs == 0 = []
                  | mod (head xs) n == 0 = (head xs):multiplosDeN n (tail xs)
                  | otherwise = multiplosDeN n (tail xs)
quitar :: Integer -> [Integer] -> [Integer] -- que elimina la primera aparicion del elemento en la lista (de haberla)
quitar n (x:xs) | length xs == 0  