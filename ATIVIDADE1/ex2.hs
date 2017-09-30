module Main where

mult3 :: Int -> Bool
mult3 x 
  | (x `rem` 3) == 0 = True
  | otherwise = False

main :: IO()
main = do
  print (mult3 8)
  print (mult3 9)
