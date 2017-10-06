module Main where

getTupla ::[Integer] -> ([Integer],[Integer])
getTupla x
   | length x `rem` 2 == 0 = (take ((length x) `div` 2) x, take ((length x) `div` 2) (reverse x))
   | otherwise = (take (((length x) +1) `div` 2) x, take ((length x) `div` 2) (reverse x))

main :: IO()
main = do
   let list = [x | x <- [1..2017], x `rem` 400 == 0 || (x `rem` 4 == 0 && x `rem` 100 /= 0)]
   print (fst (getTupla list))
   print (snd (getTupla list))
