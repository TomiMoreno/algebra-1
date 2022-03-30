doble x = 2 * x

suma x y = x + y

normaVectorial x y = sqrt(x^2 + y^2)

funcionConstante8 x = 8

absoluto:: Int -> Int
absoluto x | x >= 0 = x | otherwise = -x

maximoAbsoluto:: Int -> Int -> Int
maximoAbsoluto x y | a >= b = a
                   | otherwise = b
                   where a = abs x
                         b = abs y

maximo3:: Int -> Int -> Int -> Int
maximo3 x y z
  | x >= y = if x >= z
            then x
            else z
  | y >= z = y
  | otherwise = z

algunoEs0:: Int -> Int -> Bool 
algunoEs0 _ 0 = True
algunoEs0 0 _ = True 
algunoEs0 _ _ = False

ambosSon0:: Int -> Int -> Bool 
ambosSon0 0 0 = True
ambosSon0 _ _ = False 

esMultiploDe:: Int -> Int -> Bool 
esMultiploDe x y = mod x y == 0

digitoUnidades :: Integral a => a -> a
digitoUnidades n = mod n 10

digitoDecenas :: Integral a => a -> a
digitoDecenas n = div (mod n 100) 10