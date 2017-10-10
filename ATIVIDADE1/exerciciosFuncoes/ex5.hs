module Main where

somaDigito :: Int->Int
somaDigito x = somaDigito' x 0

somaDigito' :: Int->Int->Int
somaDigito' x y
   | x == 0 = y
   | otherwise = somaDigito' (x `div` 10) (y + (x `rem` 10))

main :: IO()
main = do
   print(somaDigito 19352)
