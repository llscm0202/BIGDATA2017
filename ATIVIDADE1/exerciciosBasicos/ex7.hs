module Main where

halfSin :: Double -> (Double, Double)
halfSin a = (sqrt ((1- cos a)/2),- sqrt ((1- cos a)/2))


main :: IO()
main = do
   print (halfSin 180)
