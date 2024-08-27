unlines :: [String] -> String
unlines [] = ""
nlines' (x:[]) = x ++ "\n"
unlines (x:y) = x ++ "\n" ++ unlines y


type Mat = [[Int]]

stringToVector :: String -> [Int]
stringToVector s = map read (words (map replaceCommas s))

replaceCommas :: Char -> Char
replaceCommas c = if c == ',' then ' ' else c

transposta :: String -> String
transposta s = unlines $ map unwords $ transpose $ map stringToVector $ lines s

-- Esta parte do código utiliza a função transpose que necessita ser importada a partir do módulo Data.List
import Data.List (transpose)

data Lista a = Esq a (Lista a) | Dir (Lista a) a | Nula

semUltimo :: Lista a -> Lista a
semUltimo(Esq_Nula)= Nula
semUltimo (Esq x y)= Esq x (semUltimo y)
SemUltimo (Dir Nula _ )= Nula
semUltimo (Dir y x) = Dir(semUltimo y) x


instance Show a => Show (Lista a) where
  show Nula = "[]"
  show (Esq x Nula) = "[" ++ show x ++ "]"
  show (Esq x xs) = "[" ++ show x ++ ", " ++ show xs ++ "]"
  show (Dir Nula x) = "[" ++ show x ++ "]"
  show (Dir xs x) = "[" ++ show xs ++ ", " ++ show x ++ "]"

  numera :: BTree a -> BTree (a,Int)
numera t = snd (numeraAux 1 t)

numeraAux :: Int -> BTree a -> (Int, BTree (a,Int))
numeraAux n Empty = (n, Empty)
numeraAux n (Node r e d) =
    let (n', left) = numeraAux n e
        (n'', right) = numeraAux (n' + 1) d
    in (n'', Node (r, n') left right)



    unInorder :: Eq a => [a] -> [BTree a]
unInorder [] = [Empty]
unInorder [x] = [Node x Empty Empty]
unInorder xs =
  [Node x left right | i <- [0..length xs - 1]
                     , x == xs !! i
                     , left <- unInorder (take i xs)
                     , right <- unInorder (drop (i + 1) xs)]
