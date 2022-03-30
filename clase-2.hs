(%) x y = x `mod` y 

estanRelacionados:: Int -> Int -> Bool
estanRelacionados a b = (a <= 3 && b <= 3) || (a > 7 && b > 7) || (a > 3 && b > 3 && a <= 7 && b <= 7)

prodInt:: Num a => (a, a) -> (a, a) -> (a, a)
prodInt (a, b) (c, d) = (a*c, b*d)

todoMenor:: Ord a => (a, a) -> (a, a) -> Bool
todoMenor (x1, x2) (y1, y2) = x1 < y1 && x2 < y2

distanciaPuntos:: Floating a => (a, a) -> (a, a) -> a
distanciaPuntos (vx, vy) (wx, wy) = sqrt((vx - wx) ** 2 + (vy - wy )**2)

sumaTerna:: Num a => (a,a,a) -> a
sumaTerna (x, y, z) = x + y + z

posicPrimerPar:: (Num a, Ord a, Integral a) => (a,a,a) -> a
posicPrimerPar (x, y, z) | x % 2 == 0 = 1
                         | y % 2 == 0 = 2
                         | z % 2 == 0 = 3
                         | otherwise = 4

crearPar:: a -> b -> (a, b)
crearPar a b = (a, b)

invertir:: (a,b) -> (b,a)
invertir (a, b) = (b, a)