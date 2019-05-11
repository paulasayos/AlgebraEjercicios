division :: Integer -> Integer -> (Integer, Integer)
division a d | a < d = (0,a)
             | otherwise = (fst (division (a-d) d ) + 1, snd (division (a-d) d))
sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta n k | k == 1 = 1
                       | n `mod` k == 0 = (sumaDivisoresHasta n (k-1)) + k
                       | otherwise = sumaDivisoresHasta n (k-1)
sumaDivisores :: Integer -> Integer
sumaDivisores n = sumaDivisoresHasta n n
--3,5
--2,5
--1,5
--0,5
-- -0,5  