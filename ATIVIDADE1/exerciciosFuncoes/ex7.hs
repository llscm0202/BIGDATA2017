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

main :: IO()
main = do
   print (coeficienteBinomial 10 5)
