module Main where

ePrimo :: Int -> Bool
ePrimo x
   | x == 1 = True
   | x == 2 = True
   | x == 3 = True
   | x `rem` 2 == 0 = False
   | otherwise = ePrimo' x 3

ePrimo' :: Int->Int-> Bool
ePrimo' x d
   | d >= truncate(sqrt (fromIntegral x)) + 1 = True
   | x `rem` d == 0 = False
   | otherwise = ePrimo' x (d+1)

main :: IO()
main = do
   let lista = [1..200]
   print (map ePrimo lista)
