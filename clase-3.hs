fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

parteEntera:: Float -> Bool -> Float
parteEntera n c
              | n < 0 = n
              | c = parteEntera(n - 1) True
              | otherwise = n + parteEntera (n - 1) True

esMultiploDe3 :: (Eq t, Num t) => t -> Bool
esMultiploDe3 0 = True
esMultiploDe3 1 = False
esMultiploDe3 2 = False
esMultiploDe3 n = esMultiploDe3(n-3)

sumaImparesRecursivo :: Integer -> Integer
sumaImparesRecursivo 1 = 1
sumaImparesRecursivo n = 2 * (n -1) + 1 + sumaImparesRecursivo(n - 1)

sumaImpares :: Integer  -> Integer
sumaImpares n = ( n - 1 ) ^ 2

medioFact :: Integer  -> Integer
medioFact 1 = 1
medioFact 0 = 1
medioFact n = n * medioFact(n -2)

sumaDigitos:: Integer -> Integer
sumaDigitos 0 = 0
sumaDigitos n = n `mod` 10 + sumaDigitos (n `div` 10)

todosLosDigitosSonIguales :: Integer -> Bool
todosLosDigitosSonIguales n
  | n < 10 = True
  | n `mod` 10 /= (n `div` 10) `mod` 10 = False
  | otherwise = todosLosDigitosSonIguales (n `div` 10)