module Main where

produtoEscalar :: [Int]->[Int] -> Int
produtoEscalar x y = sum (zipWith (*) x y)

main :: IO()
main = do
   print (produtoEscalar [1..3] [1..3])
