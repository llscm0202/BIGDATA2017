module Main where

multiplicacaoEtiope :: Int->Int->Int-> Int
multiplicacaoEtiope m n r
   | m == 1 = r+n
   | m `rem` 2 == 0 = multiplicacaoEtiope (m `div` 2)  (n * 2) r
   | otherwise = multiplicacaoEtiope (m `div` 2)  (n * 2)  (r+n)

main :: IO()
main = do
   print(multiplicacaoEtiope 14 12 0)
