inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : map (x:) (inits xs)


isPrefixOf:: Eq a => [a] -> [a] -> Bool
isPrefixOf []_ = True
isPrefixOf _[]= False
isPrefixOf (x1:y1) (x2:y2) | x1==x2 = isPrefixOf xy1 y2
                           |otherwise = False


 data BTree a = Empty
              | Node a (BTree a) (BTree a)
deriving Show

folhas :: BTree a -> Int
folhas Empty = 0 -- Árvore vazia não tem folhas
folhas (Node _ Empty Empty) = 1 -- Nó com ambos os filhos vazios é uma folha
folhas (Node _ left right) = folhas left + folhas right -- Conta as folhas nos filhos e soma

path :: [Bool] -> BTree a -> [a],
path[] Empty=[]
path []_ = []
path (x:y)(Node r e d)| x==False = r: path y e
                      |x == True = r: path y d


type Polinomio = [Coeficiente]
 type Coeficiente = Float


 valor :: Polinomio -> Float -> Float
 valor polinomio x = sum $ zipWith (\a b -> a * x ** fromIntegral b) polinomio [0..]
 

 deriv :: Polinomio -> Polinomio
deriv polinomio = zipWith (*) (tail polinomio) [0..]


soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = zipWith (+) p1 p2

type Mat a = [[a]]

quebraLinha :: [Int] -> [a] -> [[a]]
quebraLinha [] xs = [xs]  -- Se a lista de segmentos for vazia, retorna a lista original
quebraLinha _ [] = []     -- Se a linha for vazia, retorna lista vazia
quebraLinha (x:xs) ys = comp x ys : quebraLinha xs (drop x ys)

comp :: Int -> [a] -> [a]
comp 0 _ = []            -- Se o comprimento for zero, retorna lista vazia
comp _ [] = []           -- Se a lista for vazia, retorna lista vazia
comp n (z:zs) = z : comp (n - 1) zs

fragmenta :: [Int] -> [Int] -> Mat a -> [Mat a]
fragmenta _ [] _ = [] -- Se a lista de colunas for vazia, retorna lista vazia
fragmenta rows (col:cols) mat =
    map (take col) rows' : fragmenta (drop col rows) cols mat
  where
    rows' = quebraLinha rows (head mat)



    import System.Random

geraMat :: (Int, Int) -> (Int, Int) -> IO (Mat Int)
geraMat (x, y) (a, b) = do
    rows <- sequence $ replicate x (randomList y (a, b))
    return rows

randomList :: Int -> (Int, Int) -> IO [Int]
randomList n (a, b) = sequence $ replicate n (randomRIO (a, b))
