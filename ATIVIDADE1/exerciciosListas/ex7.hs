module Main where

collatzLen :: Int->Int
collatzLen x = length (listaGeradora x)

listaGeradora :: Int -> [Int]
listaGeradora x = lista
   where
      lista = (collatz x) : prox lista
      prox (x:resto)
         | x == 1 = []
         | otherwise = (collatz x) : prox resto

collatz::Int->Int
collatz x
   | x `mod` 2 == 0 = x `div` 2
   | otherwise = 3*x+1

main :: IO()
main = do
   print (collatzLen 10)
