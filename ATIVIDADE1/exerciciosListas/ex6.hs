module Main where

collatz::Int->Double
collatz x
   | x `mod` 2 == 0 = fromIntegral(x) / 2
   | otherwise = 3*fromIntegral(x)+1

main :: IO()
main = do
   print (map collatz [1..10])
