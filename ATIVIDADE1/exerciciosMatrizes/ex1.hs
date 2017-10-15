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

main :: IO()
main = do
   print(gerarMatrizIdentidade 6)
