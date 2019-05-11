rec1 :: Integer -> Integer
rec1 n | n == 1 = 3
       | otherwise = 2 * rec1(n-1) + 3^n 
factorial :: Integer -> Integer
factorial n | n == 0 = 1
            | otherwise = n * factorial(n-1)
eAprox :: Integer -> Float
eAprox n | n == 0 = 1
         | otherwise = ((fromInteger 1) / (fromInteger(factorial n))) + eAprox(n-1)
e :: Float
e = eAprox 100
parteEntera :: Float -> Integer
parteEntera n | n < 1 = 0
              | otherwise = 1 + parteEntera(n-1)
parteEnteraNegativos :: Float -> Integer
parteEnteraNegativos n | n < 0 = -parteEntera((-1)*n) -1
                       | otherwise = parteEntera(n)
division :: Integer -> Integer -> (Integer,Integer)
division a d | a < d = (0,a)
             | otherwise = (fst(division (a-d) d)+1, snd(division(a-d) d))
divisionNeg :: Integer -> Integer -> (Integer,Integer)
divisionNeg a d | a >= 0 && a < d = (0,a)
                | a >= d = (fst qr +1, snd qr)
                | a < 0 = (fst qrNeg -1, snd qrNeg)
                 where qr = divisionNeg(a-d) d
                       qrNeg = divisionNeg(a+d) d
sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta n k | k == 1 = 1
                       | snd(division n k) == 0 = k + sumaDivisoresHasta n (k-1)
                       | otherwise = sumaDivisoresHasta n (k-1)
sumaDivisores n = sumaDivisoresHasta n n
menorDivisorHasta :: Integer -> Integer -> Integer
menorDivisorHasta n k | k == 1 = n
                      | n `mod` k == 0 && k > menorDivisorHasta n (k-1) = menorDivisorHasta n (k-1)
                      | n `mod` k == 0 && k < menorDivisorHasta n (k-1) = k
                      | otherwise = menorDivisorHasta n (k-1)
menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorHasta n n
i :: Integer -> Integer -> Integer
i n m | n == 0 = 0
      | m == 0 = i (n-1) m
      | otherwise = n^m + i n (m-1)


