module Main where

concatenador :: String -> String -> String
concatenador x y = x ++ " " ++ y

main :: IO()
main = do
   print(concatenador "aoi" "uu9")
