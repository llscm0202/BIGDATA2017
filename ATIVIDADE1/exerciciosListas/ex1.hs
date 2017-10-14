module Main where

divisivel20 :: Int->Bool
divisivel20 x = divisivel20' x 1

divisivel20' :: Int->Int->Bool
divisivel20' x d
   | d > 20 = True
   | x `mod` d /= 0 = False
   | otherwise = divisivel20' x (d+1)

main :: IO()
main = do   
   print(map divisivel20 [1..3000])
