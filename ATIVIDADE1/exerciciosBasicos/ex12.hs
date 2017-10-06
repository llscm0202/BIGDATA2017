module Main where

charToInteger :: Char -> Integer
charToInteger x
   | x == '0' = 0
   | x == '1' = 1
   | x == '2' = 2
   | x == '3' = 3
   | x == '4' = 4
   | x == '5' = 5
   | x == '6' = 6
   | x == '7' = 7
   | x == '8' = 8
   | x == '9' = 9

main :: IO()
main = do
   print (map charToInteger "0123456789")
