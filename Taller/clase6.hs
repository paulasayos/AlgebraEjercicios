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
--3,5
--2,5
--1,5
--0,5
-- -0,5  