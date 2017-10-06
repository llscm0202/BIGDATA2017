module Main where

div2v :: Integer -> Double
div2v x = fromInteger x / 2

main :: IO()
main = do
   print (div2v 5)
