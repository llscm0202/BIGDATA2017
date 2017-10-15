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

main :: IO()
main = do
   print(somaDiagonal (gerarMatrizIdentidade 6))
   print(somaDiagonal ([[0,2,3],[1,4,3],[1,2,4]]))
