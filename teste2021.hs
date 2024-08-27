(\\) :: Eq a => [a] -> [a] -> [a]
(\\)[] [] = []
(\\) (x1:y1) []= (x1:y1)
(\\) (x1:y1)(x2:y2)| x1\= x2 = x1: (\\) y1 (x2:y2)
                   |otherwise = (\\) y1 y2



type MSet a = [(a,Int)]

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [(a,b):y]= [(a,b):y]
removeMSet _[]= []
removeMSet x((a,b):y)| x ==a= (a,(b-1)):y
                     | x == a && b == 1 = y
                     | otherwise = (a,b): removeMSet x y
                     
 calcula :: Eq a => MSet a -> ([a], Int)
 calcula mset = foldr acumula ([], 0) mset
          where
        acumula (x, n) (xs, total)
             | n > 0 && not (x `elem` xs) = (x : xs, total + n)
             | otherwise = (xs, total + n)
                     

 partes :: String -> Char -> [String]
 partes str c = 
  let (prefix, suffix) = break (== c) str  -- Divide a string em duas partes: antes e depois do caractere
  in if null suffix  -- Se não há mais ocorrências do caractere na string
      then [prefix]  -- Retorna a string como ela é
      else prefix : partes (drop 1 suffix) c  -- Caso contrário, adiciona o prefixo e continua a busca na substring restante

data BTree a = Empty | Node a (BTree a) (BTree a)



remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty Empty = Empty
remove x (Node r e d)
| x < r = Node r (remove x e) d  -- Se o elemento a ser removido é menor que o nó atual, remove da subárvore esquerda
| x > r = Node r e (remove x d)  -- Se o elemento a ser removido é maior que o nó atual, remove da subárvore direita
| otherwise = removeRoot (Node r e d)  -- Se o elemento a ser removido é o nó atual
where
    removeRoot (Node _ Empty d') = d'  -- Se a subárvore esquerda é vazia, retorna a subárvore direita
    removeRoot (Node _ e' Empty) = e'  -- Se a subárvore direita é vazia, retorna a subárvore esquerda
    removeRoot (Node _ e' d') = Node (leftmost d') e' (remove (leftmost d') d')  -- Se ambos os lados estão presentes
        where leftmost (Node v Empty _) = v  -- Encontra o elemento mais à esquerda na árvore
              leftmost (Node _ left _) = leftmost left



    instance show a=> Show (BTree a)
      where 
        Empty = "*"
        show (Node v Empty Empty) = "(" ++ show v ++ ")"
        show (Node v left right) = "(* " ++ show left ++ " <-" ++ show v ++ "-> " ++ show right ++ ")"


        sortOn :: Ord b => (a -> b) -> [a] -> [a]
        sortOn f xs = map snd (sortBy (\(x, _) (y, _) -> compare x y) pairs)
          where
            pairs = map (\x -> (f x, x)) xs



            fichs :: FileSystem -> [Nome]
fichs (File nome) = [nome]
fichs (Dir _ fs) = concatMap fichs fs



dirFiles :: FileSystem -> [Nome] -> Maybe [Nome]
dirFiles fs [] = Just (fichs fs)
dirFiles (Dir _ subFs) (dirName:dirs)
    | foundDir = dirFiles foundSubFs dirs
    | otherwise = Nothing
    where
        foundDir = any (\case (Dir name _) -> name == dirName; _ -> False) subFs
        foundSubFs = head [fs' | Dir name fs' <- subFs, name == dirName]
dirFiles _ _ = Nothing





listaFich :: FileSystem -> IO ()
listaFich fs = do
    putStrLn "Insira o caminho (no formato usr/xxx/PF): "
    path <- getLine
    let pathDirs = wordsWhen (== '/') path
    case dirFiles fs pathDirs of
        Just files -> mapM_ putStrLn files
        Nothing -> putStrLn "Não é uma diretoria."
    where
        wordsWhen :: (Char -> Bool) -> String -> [String]
        wordsWhen p s = case dropWhile p s of
                            "" -> []
                            s' -> w : wordsWhen p s''
                                where (w, s'') = break p s'

        
