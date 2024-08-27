zip :: [a] -> [b] -> [(a,b)]
zip [] []=[]
zip (x1:y1) []= (x1:y1)
zip [] (x2:yy2)= (x2:y2)
zip (x1:y1)(x2:y2)| (x1,x2): zip y1 y2


preCrescente :: Ord a => [a] -> [a]
preCrescente  []=[]
preCrescente (x:y)| x< head y = x:preCrescente y
                  |otherwise = x:[]

  amplitude :: [Int] -> Int
amplitude [] = 0  -- Amplitude de uma lista vazia é 0
  amplitude (x:xs) = findMinMax xs x x  -- Começamos a busca pelo menor e maior valores
                  
                  -- Função auxiliar para encontrar o menor e o maior valores na lista
 findMinMax :: [Int] -> Int -> Int -> Int
 findMinMax [] minVal maxVal = maxVal - minVal  -- Ao final da lista, calculamos a amplitude
findMinMax (x:xs) minVal maxVal = findMinMax xs (min x minVal) (max x maxVal)  -- Atualiza o menor e o maior valores conforme a lista é percorrida



type Nome = String
type Telefone = Integer
data Agenda = Vazia | Nodo (Nome,[Telefone]) Agenda Agenda

instance show a=> Show Agenda
where 
    show vazia = ""
    show Agenda = inOrder Agenda


    inOrder :: Agenda -> String
inOrder Vazia = ""
inOrder (Nodo (nome, telefones) esq dir) =
    inOrder esq ++ nome ++ ": " ++ intercalate "/" (map show telefones) ++ "\n" ++ inOrder dir


    type Mat a = [[a]]

-- Função para somar duas matrizes
soma :: Num a => Mat a -> Mat a -> Mat a
soma [] [] = []  -- Se ambas as matrizes estiverem vazias, retorna uma matriz vazia
soma (x:xs) (y:ys) = zipWith (+) x y : soma xs ys  -- Soma os elementos correspondentes das matrizes e continua a operação recursivamente
soma _ _ = error "As matrizes têm dimensões diferentes"  -- Se as matrizes não tiverem a mesma dimensão, emite um erro

import System.Random

randomSel :: Int -> [a] -> IO [a]
randomSel _ [] = return [] -- Caso a lista de entrada esteja vazia, retorna uma lista vazia (dentro do contexto IO)
randomSel 0 _ = return [] -- Se o número de elementos a serem selecionados for 0, retorna uma lista vazia (dentro do contexto IO)
randomSel n (x:xs) | n < length (x:xs) = return (x:xs) -- Se o número desejado de elementos for menor do que o tamanho da lista, retorna a lista original (dentro do contexto IO)
                   | otherwise = do
                        gen <- newStdGen -- Gera uma nova semente para números aleatórios
                        let randomIdx = take n . randomRs (0, length (x:xs) - 1) $ gen -- Gera uma lista de índices aleatórios dentro dos limites da lista
                            selected = map (x:xs !!) randomIdx -- Seleciona os elementos da lista original com base nos índices aleatórios
                        return selected
organiza :: Eq a => [a] -> [(a,[Int])]
organiza [] = []
organiza x = map(\x -> ( head x, map snd x)) groupedIndicies

 where
  indexed = zip x [0..]
  grouped = group $ sort indexed
  groupedIndicies = map (\grp -> (fst(head grp), map snd grp)) grouped


func :: [[Int]] -> [Int]
func l = concat (filter (\x -> sum x >10) l)
func []= []
func (x:y) | sum x >10 = x ++ func y
           |otherwise = func y

           data RTree a = R a [RTree a]
           type Dictionary = [ RTree (Char, Maybe String) ]

           insere :: String -> String -> Dictionary -> Dictionary
           insere palavra info dic = insere' palavra info dic
           
           insere' :: String -> String -> Dictionary -> Dictionary
           insere' [] _ dic = dic  -- Caso base: Palavra vazia, retorna o dicionário original
           insere' (c:cs) info [] = [R (c, if null cs then Just info else Nothing) (insereSubTree cs info [])]  -- Se a palavra não está presente no dicionário, insere uma nova subárvore
           insere' (c:cs) info (R (ch, desc) children : rest)
             | c == ch = R (ch, if null cs then Just info else desc) (insereSubTree cs info children) : rest  -- Se o caractere já está na árvore, continua para a subárvore correspondente
             | otherwise = R (ch, desc) children : insere' (c:cs) info rest  -- Se o caractere não é o mesmo, continua para o próximo nível da árvore
           
           insereSubTree :: String -> String -> [RTree (Char, Maybe String)] -> [RTree (Char, Maybe String)]
           insereSubTree palavra info children = insere' palavra info children


