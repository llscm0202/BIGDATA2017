module Main where

fatorial :: Int->Int
fatorial x = fatorial' x 1

fatorial' :: Int->Int->Int
fatorial' x r
   | x == 0 = r
   | x == 1 = r
   | otherwise = fatorial' (x-1) (r*x)

coeficienteBinomial :: Int -> Int -> Int
coeficienteBinomial m n = (fatorial m) `div` ( (fatorial n) * ( fatorial (m-n) ) )

elementoTrianguloPascal :: Int -> Int -> Int
elementoTrianguloPascal x y = coeficienteBinomial (x-1) (y-1)

main :: IO()
main = do
   print (elementoTrianguloPascal 7 4)

