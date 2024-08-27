data Movimento = Norte | Sul | Este | Oeste
deriving Show

hasLoops :: (Int,Int) -> [Movimento] -> Bool 
hasLoops (a,b) [] = False
hasLoops (a,b) x | elem (a,b) (aux (a,b) x) = True
				 | otherwise = False

aux:: (Int,Int) -> [Movimento] -> [(Int,Int)]
aux (a,b) [] = []
aux (a,b) (x:xs) = case x of 
  Norte -> (a,b+1):aux (a,b+1) xs
  Sul -> (a,b-1):aux (a,b-1) xs
  Este -> (a+1,b):aux (a+1,b) xs
  Oeste -> (a-1,b):aux (a-1,b) xs


 insert :: Ord a => a -> [a]-> [a] 
 insert _ [] = []
insert n ( x:y)| n<x = n:x:y
              |otherwise = x: insert n y



pos ::[a] -> Int -> a
pos (h:_) 0 = h
pos (h:t) n | n== 0 = h
            |otherwise = pos t (n -1)

isSubsequenceOf :: Eq a =>[a] -> [a] -> Bool
isSubsequenceOf [][] = True
isSubsequenceOf [] _ = False
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys)   | x == y  = isSubsequenceOf xs ys
							 	| x /= y  = isSubsequenceOf (x:xs) ys


remove ::  Eq a => [a] -> [a]-> [a]
remove [] [] = []
remove (x:y) []= (x:y)
remove (a:b) (x:y) | a==x = remove b y
                  |a /=x = x:remove y (a:b)


  heads :: [[a]] -> [a] 
  heads [[]] = []
  heads ((x:y):y2) = x: heads y2



  intersperse :: a -> [a] ->[a]
  intersperse _ [] = []
  intersperse n (x:y) = x:n:intersperse n y


  concat :: [[a]] -> [a]
  concat [] = []
  concat (x:y) = x ++ concat y

  drop :: Int -> [a] -> [a]
  drop _[] = []
  drop 0 (x:y) = (x:y)
  drop n (x:y) = drop (n-1) y



jogo :: Int -> (Int, Int) -> IO ()
jogo n (a,b) = do
  gen <-newStdGen
  let ramdomLIST = take n $ ramdomRs (a,b) gen
  putStrLn $ "lista gerada" ++ show radomList
  putStrLn "Por favor, indique um número:"
  num <- readLn :: IO Int
  if subseqSum ramdomList num
    then putStrLn "A propriedade se verificou!"
    else putStrLn "A propriedade não se verificou."



    soma :: Num a => RTree a -> a
    soma (r x []) = x
    soma (r x y) = x + sum(map soma y)
    altura :: RTree a -> Int
    alutra (r x []) = 1
    alutra (r x y) = 1 + maximum (map altura y)
    prune :: Int -> RTree a -> RTree a
    prune 0 ( R x _) = R x []
    purne n (R x y) = R x(map(prune (n-1)) y)


    mirror :: Rtree a -> RTree a
    mirror (r x y )= R x (reverse(map mirror y))

 lookup :: Eq a => a -> [(a,b)]-> Maybe b
 lookup _[] = Nothing
 lookup n ((a,b):y) | n ==a = just b
                    |othewise = lookup n y

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] [] = []
intersect (x:y) n | elem x y = x:intersect y n
                  |otherwise = intersect y n


menor :: String -> String -> Bool
menor [] [] = False
menor _ [] = False
menor [] _ = True
menor (x:xs) (y:ys) = menor xs ys



replicate :: Int -> a ->[a]
replicate 0 x = []
replicate n x | n == 0 = []
              |otherwise = x: replicate (n-1) x

 idade :: Int -> Int -> [(String,Int)] -> [String]
 idade _ _ [] = []
 idade n m ((a,b):y) | n - b >= m = a: idade n m y
                     | otherwise = idade n m y




partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' ((Left a):t) = (a:as,bs)
    where (as,bs) = partitionEithers' t
partitionEithers' ((Right b):t) = (as,b:bs)
    where (as,bs) = partitionEithers' t  



sufix :: Eq a => [a]-> [a] -> Bool
sufix [] _ = False
sufix _ [] = False
sufix l1 l2
    |l1 == l2 = True
    |l1 /= l2 = sufix l1 (tail l2)
    |otherwise = False


reverse :: [a] -> [a]
reverse [] = []
reverse (x:y) = reverse y ++ [x]

cola :: [(String,b,c)] -> String
cola [] = ""
cola ((a,b,c):y) = a++ cola y

drop :: Int -> [a] -> [a]
drop _ [] = []
drop n (x:y)| n == 0 = (x:y)
            |otherwise = drop (n-1 )y

total :: [[a]] -> Int
total [] = 0
total ([:y]) = 0 + total y
total x:y = lenght x + total y



delete :: Eq a => a -> [a]-> [a]
delete _ [] = []
delete n(x:y) | n == x = y
             |otherwise = x: delete n y


ltSum :: Num a => LTree a -> a
ltSum (tip x) = x
ltSum (Fork left right) = ltSum left + ltSum right
 

listaLT :: LTree a -> [a]
listaLT (Tip x) = [x]
listaLT (Fork left right) = listaLT left ++ listaLT right




ltHeight :: LTree a -> Int
ltheight (Tip_) = 1
ltHeight (Fork left right) = 1 + max (ltHeight left) (ltHeight right)



type Mat a = [[a]]
dimOK :: Mat a -> Bool
dimOK [] = True
dimOK(x:y)| length x == length (dimOK y) = True
          |otherwise = False




          type Mat a = [[a]]

dimOK :: Mat a -> Bool
dimOK [] = True
dimOK [_] = True -- Se houver apenas uma linha, não há como comparar com as demais
dimOK (x:y:xs) = length x == length y && dimOK (y:xs)



dimMat :: Mat a -> (Int,Int) 
dimMat [] = (0,0)
dimMat mat@(x:_) = (length mat, length x)

addMat :: Num a => Mat a -> Mat a -> Mat a~
addMat [] [] = []
addMat (x:xs) (y:ys) = zipWith (+) x y : addMat xs ys
addMat _ _ = error "As matrizes têm tamanhos diferentes"

transpose :: Mat a -> Mat a
transpose ([]:_) = []
transpose mat = map head mat : transpose (map tail mat)


multMat :: Num a => Mat a -> Mat a -> Mat a
multMat mat1 mat2 = [[sum $ zipWith (*) row col | col <- transpose mat2] | row <- mat1]


zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c 
ziWMat [] [] = []
zipWMat f (x:xs) (y:ys) = zipWith f x y : zipWMat f xs ys
zipWMat _ _ _ = error "As matrizes têm tamanhos diferentes"


triSup :: Num a => Mat a -> Bool
triSup mat = and [all (==0) (take i row) | (i, row) <- zip [0..] mat]

rotateLeft :: Mat a -> Mat a
rotateLeft = reverse . transpose


selgrau :: Int -> Polinomio -> Polinomio
selgrau grau = filter (\(_,exp) -> exp == grau)

conta :: Int -> Polinomio -> Int
conta grau = length . filter (\(_,exp) -> exp == grau)

grau :: Polinomio -> Int
grau = maximum . map snd

deriv :: Polinomio -> Polinomio
deriv = map (\(coef, exp) -> (coef * fromIntegral exp, exp - 1)) . filter (\(_, exp) -> exp /= 0)

calcula :: Float -> Polinomio -> Float
calcula x = sum . map (\(coef, exp) -> coef * (x ** fromIntegral exp))

simp :: Polinomio -> Polinomio
simp = filter (\(coef, _) -> coef /= 0)

mult :: Monomio -> Polinomio -> Polinomio
mult (coefM, expM) = map (\(coef, exp) -> (coefM * coef, expM + exp))

ordena :: Polinomio -> Polinomio
ordena = sortOn snd

normaliza :: Polinomio -> Polinomio
normaliza = nubBy (\(_,exp1) (_,exp2) -> exp1 == exp2)

soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (simp (p1 ++ p2))

produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = normaliza [(coef1 * coef2, exp1 + exp2) | (coef1, exp1) <- p1, (coef2, exp2) <- p2]

equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = normaliza p1 == normaliza p2





type MSet a = [(a,Int)]


converteMSet :: MSet a -> [a]
converteMSet [] = []
converteMSet ((a,b):y) | b \=0 = replicate b a ++ converteMSet y
                      |otherwise = converteMSet y



removeMSet :: Eq a => a -> MSet a -> MSet a
removeMSet _ [] = []
removeMSet n ((a,b):y) | n == a && b >1 = (a,(b-1)):y
                       | n == a && b == 1 = y
                       |otherwise = (a,b): removeMSet n y
                       
uniaoMSet :: Eq a => MSet a -> MSet a -> MSet a                   
uniaoMSet [] [] = []
uniaoMSet ((a1,b1):y1) ((a2,b2):y2) | a1== a2 = (a1, b1 + b2) : uniaoMSet y1 y2
                                    |otherwise = (a1,b1):insereMSet ((a2,b2):y2) y1




                                    type Posicao = (Int,Int)
                                    data Movimento = Norte | Sul | Este | Oeste
                                    data Caminho = C Posicao [Movimento]

  instance Eq Caminho Where
  (c p1 m1) == (c p2 m2) = p1 == p2 && movimentosIguais movs1 movs2
      
  movimentosIguais :: [Movimento] -> [Movimento] -> Bool
  movimentosIguais [] [] = True
  movimentosIguais _ [] = False
  movimentosIguais [] _ = False
  movimentosIguais (m1:ms1) (m2:ms2) = m1 == m2 && movimentosIguais ms1 ms2

  func :: [[Int]] -> [Int]
func l = concat (filter (\x -> sum x >10) l)

func [] = []
func (x:y) | sum x >10 = [x]++ func y
           |otherwise = func y



           data Prop = Var String | Not Prop | And Prop Prop | Or Prop Prop
           p1 :: Prop
           p1 = Not (Or (And (Not (Var "A")) (Var "B")) (Var "C"))





-- Função auxiliar que obtém o valor lógico de uma variável
getVarValue :: String -> [(String, Bool)] -> Bool
getVarValue _ [] = False -- Se a variável não estiver no conjunto, retorna False
getVarValue var ((v, val):vs)
  | var == v = val -- Se encontrarmos a variável, retornamos seu valor
  | otherwise = getVarValue var vs -- Caso contrário, continuamos procurando

-- Função para avaliar uma expressão proposicional
eval :: [(String, Bool)] -> Prop -> Bool
eval _ (Var x) = getVarValue x -- Avalia uma variável
eval vals (Not p) = not (eval vals p) -- Avalia a negação
eval vals (And p q) = eval vals p && eval vals q -- Avalia o "E"
eval vals (Or p q) = eval vals p || eval vals q -- Avalia o "OU"
 

 nnf :: Prop -> Prop q
 nnf (var x)= var x
 nnf (Not(Not x))= nnf x
 nnf(Not(Or x y)) = And ( (nnf (Not x)) (nnf ( not y)))
 nnf (not(and x y)) = Or (nnf( not x)) (nnf(not y))
 nnf (and x y) = and(nnf x) (nnf y)
 nnf (or x y) = or (nnf x) (nnf y)



 unlines :: [String] -> String
 unlines [] = ""
 unlines (x:[] )=x++"\n"
 unlines(x:y) = x ++ "\n"++ unlines y


 stringToVector :: String -> [Int]
 stringToVector s = map read (words (map replaceCommas s))

replaceCommas :: Char -> Char
replaceCommas c = if c == ',' then ' ' else c


transposta :: String -> String
transposta s = unlines $ map unwords $ transpose $ map stringToVector $ lines s

data Lista a = Esq a (Lista a) | Dir (Lista a) a | Nula


semUltimo :: Lista a -> Lista a
semUltimo (Esq Nula) = Nula
semUltimo (Esq x y) =Esq x (SemUltimo y)
semUltimo (dir y x) = Dir(semUltimo y)  x

instance Show a =>Show (Lista a) where
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


replicate :: Int -> a -> [a]    
replicate _ [] = []
replicate x n | x == 0 = []
              |otherwise = [n] ++ replicate (x-1) n


intersect :: Eq a => [a] -> [a] -> [a]
intersect  [] [] = []
intersect (x1:y1) [] = (x1:y1)
intersect (x1:y1) (x2:y2) | x1 =\x2 = intersect y1 y2 
                          |otherwise = x1: intersect y1 (x2:y2)




                          data LTree a = Tip a | Fork (LTree a) (LTree a)
                          data FTree a b = Leaf a | No b (FTree a b) (FTree a b)

conv :: LTree Int -> FTree Int Int
conv Tip a = Leaf a
conv (Fork a b) = No (sumLeaves (Fork a b)) (conv a) (conv b)


sumLeaves :: LTree Int -> Int
    sumLeaves (Tip y) = y  -- Se for uma folha, retorna seu valor
    sumLeaves (Fork l r) = sumLeaves l + sumLeaves r  -- Soma os valores das folhas nas subárvores esquerda e direita



    type Mat a = [[a]]

    triSup :: Num a => Mat a -> Bool
    triSup mat = allZerosBelowDiagonal mat 0
    
    allZerosBelowDiagonal :: Num a => Mat a -> Int -> Bool
    allZerosBelowDiagonal [] _ = True
    allZerosBelowDiagonal (row:rows) idx = all (== 0) (drop idx row) && allZerosBelowDiagonal rows (idx + 1)
    





data SReais = AA Double Double | FF Double Double
| AF Double Double | FA Double Double
| Uniao SReais SReais

instance Show SReais where

Show (AA x y) = "]" ++ show x ++ "," ++ show y ++"["
Show (FF x y) = "[" ++ show x ++ ","++ show y ++ "]"
Show (AF x y ) = "]" ++ show x ++ "," ++ show y ++ "]"
show (FA x y) = "[" ++ show x ++ "," ++ show y ++ "["
show (uniao x y) = "(" ++ show x ++ " U " ++ show y ++ ")"

tira :: Double -> SReais -> SReais 
tira _ (AA x y) = AA x y
tira _ (FF x y) = FF x y
tira _ (AF x y) = if x == y then FA x y else AF x y
tira _ (FA x y) = if x == y then AF x y else FA x y
tira n (Uniao a b) = Uniao (tira n a) (tira n b)



unc :: Float -> [(Float,Float)] -> [Float]
func x l = map snd (filter ((>x) . fst) l)
func n ((a,b):y) | a>n = b: func y
                 |otherwise = func n y



subseqSum :: [Int] -> Int -> Bool 
subseqSum [] 0 = True
subseqSum [] _ = False
subseqSum (x:y) n | x> n = subseqSum y n
                  |otherwise = subseqSum y (n - x) || subseqSum y n




jogo :: Int -> (Int, Int) -> IO ()
jogo n (a b) = do
  rnd <- sequence $ replicate n (randomRIO (a, b))
  putStrLn $ "Lista gerada: " ++ show randList
   
   putStrLn "Insira um número para verificar se existe uma sub-sequência com essa soma:"
   num <- readLn
   
   let hasSubseq = subseqSum randList num -- Verifica se a lista gerada possui uma sub-sequência com a soma num
   
   putStrLn $ if hasSubseq then "A lista possui uma sub-sequência com a soma " ++ show num
                           else "A lista não possui uma sub-sequência com a soma " ++ show num






zip :: [a] -> [b] -> [(a,b)]
zip [] [] = []
zip (x1:y1) []= (x1:y1)
zip [] (x2:yy2)= (x2:y2)
zip (x1:y1) (x2:y2) = (x1,x2): zip y1 y2



preCrescente :: Ord a => [a] -> [a]
reCrescente  []=[]
preCrescente (x:y)| x< head y = x:preCrescente y
                  |otherwise = x:[]


                  amplitude :: [Int] -> Int
                  amplitude [] = 0  -- Amplitude de lista vazia é 0
                  amplitude lst = maximum lst - minimum lst

                  



 type Mat a = [[a]] 
 soma :: Num a => Mat a -> Mat a -> Mat a
 soma (x1:y1) (x2:y2) = zipwith (+) x y : soma y1 y2



 type Nome = String
type Telefone = Integer
data Agenda = Vazia | Nodo (Nome,[Telefone]) Agenda Agenda

instance Show a => Show Agenda
where
  Show vazia = ""
  show Agenda = order Agenda


  order :: Agenda -> String
  order vazia = ""
  order (node (n,t)e d) = order e ++ n ++ ":" ++ intercalate "/" (map show t) ++ "\n" ++ order d


  randomSel :: Int -> [a] -> IO [a]
  andomSel _ [] = return [] -- Caso a lista de entrada esteja vazia, retorna uma lista vazia (dentro do contexto IO)
  randomSel 0 _ = return [] -- Se o número de elementos a serem selecionados for 0, retorna uma lista vazia (dentro do contexto IO)
  randomSel n (x:xs) | n < length (x:xs) = return (x:xs) -- Se o número desejado de elementos for menor do que o tamanho da lista, retorna a lista original (dentro do contexto IO)
                     | otherwise = do
                          gen <- newStdGen -- Gera uma nova semente para números aleatórios
                          let randomIdx = take n . randomRs (0, length (x:xs) - 1) $ gen -- Gera uma lista de índices aleatórios dentro dos limites da lista
                              selected = map (x:xs !!) randomIdx -- Seleciona os elementos da lista original com base nos índices aleatórios
                          return selected



 ganiza :: Eq a => [a] -> [(a,[Int])]
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


remove :: Eq a => [a] -> [a] -> [a]
remove (x:y) [] = (x:y)
remove [] [] = []
remove (x1:y1) (x2:y2) | x1 == x2 = remvove y1 y2
                       | otherwise= x1: remove y1 (x2:y2)

  

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet_ [] = []
removeMSet n ((a,b):y) | n==a && b ==1 = y
                       |n==a && b>1 = (a,b-1):y
                       |otherwise= (a,b): removeMSet n y




                       calcula :: MSet a -> ([a], Int)
                       calcula [] = ([], 0)
                       calcula ((a, b):y) = ([a] ++ xs, soma)
                         where (xs, total) = calcula y
                               soma = b + total
                       
     data BTree a = Empty | Node a (BTree a) (BTree a)

remove :: Ord a => a -> BTree a -> BTree a 
remove _ Empty Empty = Empty
remove x (Node r e d)
|x < r = node r (remove x e) d
|x> r = node r e ( remove x d)
|otherwise = removeRoot (Node  r e d)
where
  removeRoot (Node_ Empty d') = d'
  removeRoot (Node _ e' Empty) = e'




  type Nome = String
  type Numero = Int
  type NT = Maybe Float
  type NP = Maybe Float
  data Regime = Ordinario | TrabEstud
  deriving (Show, Eq)
  type Aluno = (Numero, Nome, Regime, NT, NP)
  type Turma = [Aluno]





  pesquisaAluno :: Numero -> Turma -> Maybe Aluno
  pesquisaAluno _ [] = Nothing
  pesquisaAluno n ((num,nome,re,nt,np):y) | n== num = Just nome
  |otherwise = pesquisaAluno n y



  alteraNP :: Numero -> NP -> Turma -> Turma
  alteraNP _ []
  alteraNp n x ((num,nome,re,nt,np):y) |n == num = (num,nome,re,nt,x):y
  |otherwise =alteraNp n x y


  comparaNumero :: Aluno -> Aluno -> Ordering
comparaNumero (num1, _, _, _, _) (num2, _, _, _, _) = compare num1 num2

-- Função de inserção ordenada
insere :: Aluno -> Turma -> Turma
insere novoAluno turma = insertBy comparaNumero novoAluno turma




  data BTree a = Empty | Node a (BTree a) (BTree a)
deriving Show



type Turma = BTree Aluno

insere :: Aluno -> Turma -> Turma
insere _ Empty = Empty
insere (n,nom,re,nt,np) (node(n1,nom1,re1,nt1,np1) e d)| n< n1 = node (n1,nom1,re1,nt1,np1) (insere(n,nom,re,nt,np) e) d
|n>n1 = node(n1,nom1,re1,nt1,np1) e (insere(n,nom,re,nt,np) d)

pesquisaAluno :: Numero -> Turma -> Maybe Aluno
pesquisaAluno _ Empty = nothing
pesquisaAluno x (node(n1,nom1,re1,nt1,np1) e d) | x == n1 = just (n1,nom1,re1,nt1,np1)
| x < n1 = pesquisaAluno x e 
|otherwise = pesquisaAluno x d

alteraNP :: Numero -> NP -> Turma -> Turma 
alteraNP _ Empty = Empty
alteraNP x node((n1,nom1,re1,nt1,np1) e d) | x == np1 = node (n1,nom1,re1,nt1,x) e d
| x< np1 = alteraNP x e
|otherwise = alteraNP x d



data OP = SOMA | SUB | PROD | DIV
deriving (Show, Eq)
data Expr = Folha Int | Nodo OP Expr Expr
deriving (Show, Eq)


aplica :: OP -> Int -> Int -> Int
aplica SOMA x y = x + y
aplica SUB x y = x- y
aplica PROD x y = x* y
aplicaq DIV x y = div x y


avalia :: Expr -> Int
avalia Node (x e d) = aplica x (avalida e) (avalia d)


imprime :: Expr -> String
imprime (Folha n) = show n
imprime (Nodo op esq dir) =
  "(" ++ imprime esq ++ " " ++ mostraOp op ++ " " ++ imprime dir ++ ")"
  where
    mostraOp SOMA = "+"
    mostraOp SUB = "-"
    mostraOp PROD = "*"
    mostraOp DIV = "/"



    listaAlunosIO :: Turma -> IO ()
    listaAlunosIO turma = do
      putStrLn "Lista de Alunos:"
      putStrLn "================="
      putStrLn (listaAlunos turma)
      putStrLn ""


      procuraAlunoIO :: Turma -> IO ()
      procuraAlunoIO turma = do
        putStr "numero do aluno"
        numeroStr <- getline
        let numero = read numeroStr :: Numero
        case of pesquisaAluno numero turma of
          just aluno -> putstrln $ "aluno" ++ show aluno
          Nothing-> putStrln "nao existe"