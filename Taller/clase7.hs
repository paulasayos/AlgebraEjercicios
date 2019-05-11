lothar :: Integer -> Integer 
lothar an = snd(lotharAux (an, 1))
lotharAux :: (Integer, Integer) -> (Integer, Integer)
lotharAux (an, k) | an == 1 = (1, k)
               | otherwise = lotharAux(lotharAn an, k+1)
lotharAn :: Integer -> Integer
lotharAn an | an `mod` 2 == 0 = an `div` 2
            | otherwise = 3 * an + 1
maximoLotharDesde :: (Integer, Integer) -> (Integer, Integer)
maximoLotharDesde (n, k) | k == 0 = (n, lothar n)
                 | lothar n >= lothar (k) = maximoLotharDesde(n ,(k-1))
                 | otherwise = maximoLotharDesde(k,(k-1))
maximoLothar :: Integer -> (Integer, Integer)
maximoLothar n = maximoLotharDesde(n, n)



