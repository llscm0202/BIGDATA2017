module Main where

lista = 1 : 2 : prox lista
   where
      prox (x:t@(y:resto)) = (x+y):prox t

main ::IO()
main = do
   print ( sum [x| x <- (takeWhile (< 4000001) lista),x`mod`2==0])
