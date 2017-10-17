module Main where
import Data.List
data Nacionalidade = Norwegian | Englishman | Spaniard | Ukrainian | Japanese deriving (Show,Enum,Eq,Ord)
data Casa = Red | Green | Ivory | Yellow | Blue deriving (Show,Enum,Eq,Ord)
data Bebida = Milk | Coffee | Tea |  OrangeJuice | Water deriving (Show,Enum,Eq,Ord)
data Bicho = Dog | Snails | Fox | Horse | Zebra deriving (Show,Enum,Eq,Ord)
data Cigarro = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments deriving (Show,Enum,Eq,Ord)
data Posicao = Posicao0 | Posicao1 | Posicao2 | Posicao3 | Posicao4 deriving (Show,Enum,Eq,Ord)

data Registro = Registro {posicao::Posicao, nacionalidade::Nacionalidade, casa::Casa, bebida::Bebida, bicho::Bicho, cigarro::Cigarro} deriving (Show,Eq)

data Solucao = Solucao {index0::Registro, index1::Registro, index2::Registro, index3::Registro, index4::Registro} deriving (Show)

eq :: Registro -> Registro -> Bool
eq registroA registroB 
   | posicao registroA == posicao registroB || nacionalidade registroA == nacionalidade registroB || casa registroA == casa registroB || bebida registroA == bebida registroB || bicho registroA == bicho registroB || cigarro registroA == cigarro registroB = True
   | otherwise = False

validar2 :: Registro -> Bool
validar2 registro
   | nacionalidade registro == Englishman && casa registro /= Red = False
   | nacionalidade registro /= Englishman && casa registro == Red = False
   | otherwise = True

validar3 :: Registro -> Bool
validar3 registro
   | nacionalidade registro == Spaniard && bicho registro /= Dog = False
   | nacionalidade registro /= Spaniard && bicho registro == Dog = False
   | otherwise = True

validar4 :: Registro -> Bool
validar4 registro
   | bebida registro == Coffee && casa registro /= Green = False
   | bebida registro /= Coffee && casa registro == Green = False
   | otherwise = True

validar5 :: Registro -> Bool
validar5 registro
   | nacionalidade registro == Ukrainian && bebida registro /= Tea = False
   | nacionalidade registro /= Ukrainian && bebida registro == Tea = False
   | otherwise = True

validar6 :: [Registro] -> Bool
validar6 registros
   | fromEnum (posicao casaGreen) == fromEnum (posicao casaIvory) + 1 = True
   | otherwise = False
   where
      casaGreen = (dropWhile (\x -> casa x /= Green) registros)!!0
      casaIvory = (dropWhile (\x -> casa x /= Ivory) registros)!!0      

validar7 :: Registro -> Bool
validar7 registro
   | cigarro registro == OldGold && bicho registro /= Snails = False
   | cigarro registro /= OldGold && bicho registro == Snails = False
   | otherwise = True

validar8 :: Registro -> Bool
validar8 registro
   | cigarro registro == Kools && casa registro /= Yellow = False
   | cigarro registro /= Kools && casa registro == Yellow = False
   | otherwise = True

validar9 :: Registro -> Bool
validar9 registro
   | bebida registro == Milk && posicao registro /= Posicao2 = False
   | bebida registro /= Milk && posicao registro == Posicao2 = False
   | otherwise = True

validar10 :: Registro -> Bool
validar10 registro
   | nacionalidade registro == Norwegian && posicao registro /= Posicao0 = False
   | nacionalidade registro /= Norwegian && posicao registro == Posicao0 = False
   | otherwise = True

validar11 :: [Registro] -> Bool
validar11 registros
   | fromEnum (posicao cigarroChesterfields) == fromEnum (posicao bichoFox) + 1 = True
   | otherwise = False
   where
      cigarroChesterfields = (dropWhile (\x -> cigarro x /= Chesterfields) registros)!!0
      bichoFox = (dropWhile (\x -> bicho x /= Fox) registros)!!0

validar12 :: [Registro] -> Bool
validar12 registros
   | fromEnum (posicao cigarroKools) == fromEnum (posicao bichoHorse) -1 = True
   | otherwise = False
   where
      cigarroKools = (dropWhile (\x -> cigarro x /= Kools) registros)!!0
      bichoHorse = (dropWhile (\x -> bicho x /= Horse) registros)!!0

validar13 :: Registro -> Bool
validar13 registro
   | cigarro registro == LuckyStrike && bebida registro /= OrangeJuice = False
   | cigarro registro /= LuckyStrike && bebida registro == OrangeJuice = False
   | otherwise = True

validar14 :: Registro -> Bool
validar14 registro
   | nacionalidade registro == Japanese && cigarro registro /= Parliaments = False
   | nacionalidade registro /= Japanese && cigarro registro == Parliaments = False
   | otherwise = True

validar15 :: [Registro] -> Bool
validar15 registros
   | fromEnum (posicao nacionalidadeNorwegian) == fromEnum (posicao casaBlue) -1 = True
   | otherwise = False
   where
      nacionalidadeNorwegian = (dropWhile (\x -> nacionalidade x /= Norwegian) registros)!!0
      casaBlue = (dropWhile (\x -> casa x /= Blue) registros)!!0

--validar usando apenas as regras clássicas
validar :: [Registro] -> [Registro]
validar r = filter validar2 $ filter validar3 $ filter validar4 $ filter validar5 $ filter validar7 $ filter validar8 $ filter validar8 $ filter validar9 $ filter validar10 $ filter validar13 $ filter validar14 r-- $ filter validarExtra1 $ filter validarExtra2 $ filter validarExtra3 $ filter validarExtra4 r

--validar rápido
--validar :: [Registro] -> [Registro]
--validar r = filter validar2 $ filter validar3 $ filter validar4 $ filter validar5 $ filter validar7 $ filter validar8 $ filter validar8 $ filter validar9 $ filter validar10 $ filter validar13 $ filter validar14 $ filter validarExtra1 $ filter validarExtra2 $ filter validarExtra3 $ filter validarExtra4 r

validarExtra1 :: Registro -> Bool
validarExtra1 registro
   | casa registro == Blue && posicao registro /= Posicao1 = False
   | casa registro /= Blue && posicao registro == Posicao1 = False
   | otherwise = True

validarExtra2 :: Registro -> Bool
validarExtra2 registro
   | casa registro == Yellow && posicao registro /= Posicao0 = False
   | casa registro /= Yellow && posicao registro == Posicao0 = False
   | otherwise = True

validarExtra3 :: Registro -> Bool
validarExtra3 registro
   | cigarro registro == Kools && posicao registro /= Posicao0 = False
   | cigarro registro /= Kools && posicao registro == Posicao0 = False
   | otherwise = True

validarExtra4 :: Registro -> Bool
validarExtra4 registro
   | bicho registro == Horse && posicao registro /= Posicao1 = False
   | bicho registro /= Horse && posicao registro == Posicao1 = False
   | otherwise = True

main :: IO()
main = do
   let r2 = [Registro (toEnum x5::Posicao) (toEnum x0::Nacionalidade) (toEnum x1::Casa) (toEnum x2::Bebida) (toEnum x3::Bicho) (toEnum x4::Cigarro) | x0 <- [0..4],x1 <- [0..4],x2 <- [0..4],x3 <- [0..4],x4 <- [0..4], x5<-[0..4]]

   let t1 = validar r2

   let t2 = [ [c1, c2, c3, c4, c5] | c1<-t1,c2<-t1,c3<-t1,c4<-t1,c5<-t1,posicao c1 == Posicao0, posicao c2 == Posicao1, posicao c3 == Posicao2, posicao c4 == Posicao3, posicao c5 == Posicao4,c1 `eq` c2 == False, c1 `eq` c3 == False, c1 `eq` c4 == False, c1 `eq` c5 == False, c2 `eq` c3 == False, c2 `eq` c4 == False, c2 `eq` c5 == False, c3 `eq` c4 == False, c3 `eq` c5 == False, c4 `eq` c5 == False, validar6 [c1,c2,c3,c4,c5], validar11 [c1,c2,c3,c4,c5], validar12 [c1,c2,c3,c4,c5], validar15 [c1,c2,c3,c4,c5]]
   print(t2!!0)
