module Main where

mult35 :: Int -> Bool
mult35 x
  | x `rem` 5 == 0 && x `rem` 3 == 0 = True
  | otherwise = False

main :: IO()
main = do
  print (mult35 15)
  print (mult35 34)
