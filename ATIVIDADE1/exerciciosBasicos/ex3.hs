module Main where

mult5 :: Int -> Bool
mult5 x 
  | x `rem` 5 == 0 = True
  | otherwise = False

main :: IO()
main = do

  print (mult5 10)
  print (mult5 12)
