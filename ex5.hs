module Main where

myFunction x
  | x < -1 = True
  | x > 1 && x `rem` 2 == 0 = True
  | otherwise = False

main :: IO()
main = do
  inputUser <- getLine
  let a = (read inputUser :: Int)
  print (myFunction a)
