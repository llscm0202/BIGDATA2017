module Main where
import Data.List

tipoTriangulo :: [Integer] -> String
tipoTriangulo listaAux
   | zeroUmIguais && umDoisIguais = "triangulo equilatero"
   | zeroUmIguais || umDoisIguais = "triangulo isosceles"
   | otherwise = "triangulo escaleno"
   where 
      lista = sort listaAux
      zeroUmIguais = (lista !! 0 == lista !! 1)
      umDoisIguais = (lista !! 1 == lista !! 2)

main :: IO()
main = do
   let l = [1,3,2]
   let m = [2,3,2]
   let n = [3,3,3]
   print (tipoTriangulo l)
   print (tipoTriangulo m)
   print (tipoTriangulo n)
