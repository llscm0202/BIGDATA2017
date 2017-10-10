module Main where

somaDigito :: Int->Int
somaDigito x = somaDigito' x 0

somaDigito' :: Int->Int->Int
somaDigito' x y
   | x == 0 = y
   | otherwise = somaDigito' (x `div` 10) (y + (x `rem` 10))

persistenciaAditiva :: Int -> Int
persistenciaAditiva x = persistenciaAditiva' x 0

persistenciaAditiva' :: Int -> Int -> Int
persistenciaAditiva' x y
   | x `div` 10 == 0 = y
   | otherwise = persistenciaAditiva' (somaDigito x) (y+1)

main :: IO()
main = do
   print (persistenciaAditiva 1)
   print (persistenciaAditiva 9)
   print (persistenciaAditiva 1233)
   print (persistenciaAditiva 1234)
