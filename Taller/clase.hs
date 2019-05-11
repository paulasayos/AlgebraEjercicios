f x y = x * x + y * y
g x y z = x + y + z * z
doble x = x * 2
suma x y = x + y
normaVectorial x y = sqrt(x**2 + y**2) 
funcionConstante8 x = 8
respuestaATodo = 42
signo n | n > 0 = 1
        | n == 0 = 0
        | n < 0 = (-1)
valorAbsoluto n | n<0 = (-n)
    |n>0 = n
    |n== 0 = 0
valorAbsoluto2 m = m * signo m
maximo2 x y | x > y = x
            | otherwise = y 
maximo3 x y z = maximo2 (maximo2 x y ) z 

