type MSet a = [(a,Int)]


converteMSet :: MSet a -> [a]
converteMSet [] = []
converteMSet ((a,b):y) | b /= 0 = replicate' b a ++ converteMSet y
| otherwise = converteMSet y

removeMSet :: Eq a => a -> MSet a -> MSet a
removeMSet _ [] = []
removeMSet n ((a,b):y)
    | n == a && b > 1 = (a, b - 1) : y
    | n == a && b == 1 = y
    | otherwise = (a, b) : removeMSet n y


    uniaoMSet :: Eq a => MSet a -> MSet a -> MSet a
    uniaoMSet m1 m2 = foldr (\(x,n)acc -> insereMSet x n acc )m2 m1
    insereMSet :: Eq a => a -> Int -> MSet a -> MSet a
insereMSet x n [] = [(x, n)]
insereMSet x n ((y, m):rest)
    | x == y = (y, n + m) : rest
    | otherwise = (y, m) : insereMSet x n rest


    type Posicao = (Int,Int)
    data Movimento = Norte | Sul | Este | Oeste
    data Caminho = C Posicao [Movimento]
    instance Eq Caminho where
        (C pos1 movs1) == (C pos2 movs2) = pos1 == pos2 && movimentosIguais movs1 movs2
      
      movimentosIguais :: [Movimento] -> [Movimento] -> Bool
      movimentosIguais [] [] = True
      movimentosIguais _ [] = False
      movimentosIguais [] _ = False
      movimentosIguais (m1:ms1) (m2:ms2) = m1 == m2 && movimentosIguais ms1 ms2


      func :: [[Int]] -> [Int]
      func []= []
      func (x:y) | sum x >10 = [x] ++ func y
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

nnf :: Prop -> Prop
nnf (Var x) = Var x
nnf (Not(Not p)) = nnf p
nnf(Not(And p q)) = Or (nnf(Not p)) (nnf(Not q))
nff(Not(Or p q)) = And (nnf(Not p)) (nnf(Not q))
nnf (Not p) = Not (nnf p) -- Se for uma negação isolada, mantém a negação
nnf (And p q) = And (nnf p) (nnf q) -- Continua avaliando o "E"
nnf (Or p q) = Or (nnf p) (nnf q) 