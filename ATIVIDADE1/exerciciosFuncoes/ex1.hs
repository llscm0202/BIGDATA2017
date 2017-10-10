module Main where

import Data.List

ehTrianguloAux :: [Integer] -> Bool
ehTrianguloAux listaOrdenada
   | (listaOrdenada!!0) + (listaOrdenada!!1) > (listaOrdenada!!2) = True
   | otherwise = False

ehTriangulo :: [Integer]-> Bool
ehTriangulo lista = ehTrianguloAux (sort lista)

main :: IO()
main = do
   let l = [5,3,4]
   let m = [5,4,1]
   print (ehTriangulo l)
   print (ehTriangulo m)

