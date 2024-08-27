replicate :: Int -> a -> [a]
replicate _ n=[]
replicate x n | x== 0 = []
              |otherwise = n:replicate (x-1) n

intersect :: Eq a => [a] -> [a] -> [a]
intersect []_ = []
intersect _[] = []
intersect [][]= []
intersect (x1:y1)y| x1 elem y= x1: intersect y1 y
                  |otherwise = intersect y1 y

data LTree a = Tip a | Fork (LTree a) (LTree a)
 data FTree a b = Leaf a | No b (FTree a b) (FTree a b)




 conv :: LTree Int -> FTree Int Int
conv (Tip x) = Leaf x  -- Se for uma folha, retorna uma folha com o valor x
conv (Fork left right) = No (sumLeaves (Fork left right)) (conv left) (conv right)
  where
    sumLeaves :: LTree Int -> Int
    sumLeaves (Tip y) = y  -- Se for uma folha, retorna seu valor
    sumLeaves (Fork l r) = sumLeaves l + sumLeaves r  -- Soma os valores das folhas nas subárvores esquerda e direita

    type Mat a = [[a]]

    triSup :: Num a => Mat a -> Bool
    triSup mat = allZerosBelowDiagonal (zipWith drop [0..] mat)

    allZerosBelowDiagonal :: Num a => Mat a -> Bool
    allZerosBelowDiagonal [] = True  -- Se a matriz estiver vazia, é triangular superior por definição
    allZerosBelowDiagonal (row:rest) = allZerosBelowRow 0 row && allZerosBelowDiagonal rest
      where
        allZerosBelowRow :: Num a => Int -> [a] -> Bool
        allZerosBelowRow _ [] = True  -- Se a linha estiver vazia, é triangular superior por definição
        allZerosBelowRow index (x:xs)
          | index > 0 && x /= 0 = False  -- Se um elemento abaixo da diagonal não for zero, não é triangular superior
          | otherwise = allZerosBelowRow (index + 1) xs


          data SReais = AA Double Double | FF Double Double
          | AF Double Double | FA Double Double
          | Uniao SReais SReais

          instance Show SReais where
            show (FF a b) = "[" ++ show a ++ "," ++ show b ++ "]"
            show (AA a b) = "]" ++ show a ++ "," ++ show b ++ "["
            show (AF a b) = "]" ++ show a ++ "," ++ show b ++ "]"
            show (FA a b) = "[" ++ show a ++ "," ++ show b ++ "["
            show (Uniao x y) = "(" ++ show x ++ " U " ++ show y ++ ")"

            tira :: Double -> SReais -> SReais
            tira _ (AA x y) = AA x y
            tira _ (FF x y) = FF x y
            tira _ (AF x y) = if x == y then FA x y else AF x y
            tira _ (FA x y) = if x == y then AF x y else FA x y
            tira n (Uniao a b) = Uniao (tira n a) (tira n b)
          
 func :: Float -> [(Float,Float)] -> [Float]
 func _ [] = []
 func x ((a,b):y)| a>b: func x y
                 |otherwise = func x y


subseqSum :: [Int] -> Int -> Bool
subseqSum nums target = dp nums target
   where
   dp :: [Int] -> Int -> Bool
   dp [] 0 = True  -- Se a soma desejada for zero e a lista estiver vazia, retorna True
  dp [] _ = False  -- Se a lista estiver vazia e a soma não for zero, retorna False
  dp (x:xs) k
 | x > k = dp xs k  -- Se o elemento atual for maior que a soma desejada, continue sem incluí-lo
 | otherwise = dp xs (k - x) || dp xs k  -- Verifique se é possível obter a soma desejada incluindo ou excluindo o elemento atual

 jogo :: Int -> (Int, Int) -> IO ()
 jogo n (a, b) = do
   randList <- sequence $ replicate n (randomRIO (a, b)) -- Gera a lista aleatória de tamanho n com números entre a e b
   putStrLn $ "Lista gerada: " ++ show randList
   
   putStrLn "Insira um número para verificar se existe uma sub-sequência com essa soma:"
   num <- readLn
   
   let hasSubseq = subseqSum randList num -- Verifica se a lista gerada possui uma sub-sequência com a soma num
   
   putStrLn $ if hasSubseq then "A lista possui uma sub-sequência com a soma " ++ show num
                           else "A lista não possui uma sub-sequência com a soma " ++ show num