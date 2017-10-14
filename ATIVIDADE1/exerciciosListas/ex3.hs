module Main where

lista = 1 : 2 : prox lista
   where
      prox (x:t@(y:resto)) = (x+y):prox t

main ::IO()
main = do
   print ( take 10 lista)
