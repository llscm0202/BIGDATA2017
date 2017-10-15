module Main where

gerarLinha :: Int -> Int -> Int -> [Int] -> [Int]
gerarLinha x xA n listaI
   | xA >= n = listaI
   | otherwise =  (funcaoAuxiliar x xA) : (gerarLinha x (xA+1) n listaI)

funcaoAuxiliar :: Int -> Int -> Int
funcaoAuxiliar x xA
   | x == xA = 1
   | otherwise = 0

gerarMatrizIdentidade :: Int -> [[Int]]
gerarMatrizIdentidade n = [gerarLinha x 0 n [] | x<-[0..n-1]]

somaDiagonal :: [[Int]] -> Int
somaDiagonal matriz = sum [(matriz !! x)!!x | x <- [0..((length matriz)-1)]]

somaDiagonalSecundaria :: [[Int]] -> Int
somaDiagonalSecundaria matriz = sum [(matriz !! x)!!(((length matriz)-1)-x) | x <- [0..((length matriz)-1)]]

main :: IO()
main = do

   print(somaDiagonalSecundaria (gerarMatrizIdentidade 6))
   print(somaDiagonalSecundaria (gerarMatrizIdentidade 7))
   print(somaDiagonalSecundaria ([[1,2,3],[4,5,6],[7,8,9]]))

