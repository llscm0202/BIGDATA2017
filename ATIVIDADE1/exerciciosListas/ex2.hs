module Main where

projectEuler5 :: Int
projectEuler5 = projectEuler5' 11 12

projectEuler5' :: Int -> Int -> Int
projectEuler5' x i
   | i>20 = x
   | otherwise = projectEuler5' (lcm x i) (i+1)

main :: IO()
main = do      
   print(projectEuler5)
