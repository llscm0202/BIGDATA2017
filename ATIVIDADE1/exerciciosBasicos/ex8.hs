module Main where

main :: IO()
main = do
   let list = [x | x <- [1..2017], x `rem` 400 == 0 || (x `rem` 4 == 0 && x `rem` 100 /= 0)]
   print (list)
