intersect:: Eq a => [a] -> [a] -> [a]
intersect []_= []
intersect[][]=[]
intersect _[]= []
intersect (x1:y1)y2| x1==elem y2 = x1: intersect  y1 y2
                        |otherwise= intersect y1 y2


 tails :: [a] -> [[a]]
 tails []= [[]]
 tails (x:y)= (x:y):tails y
 
type ConjInt = [Intervalo]
type Intervalo = (Int,Int)

elems :: ConjInt -> [Int]
elems [] = []

elems((a,b):y)| b == a +1 = a:b:elems y
              | a ==b = a:b:elems y
              | b >=a+1 = a:b-1:elems y

              
 elems :: ConjInt -> [Int]
  elems conj = concatMap (\(x, y) -> [x..y]) conj
              


  geraconj :: [Int] -> ConjInt
  geraconj [] = []
  geraconj (x:xs) = reverse $ geraIntervalo xs [(x, x)]
  
  geraIntervalo :: [Int] -> ConjInt -> ConjInt
  geraIntervalo [] acc = acc
  geraIntervalo (x:xs) ((a, b):rest)
      | x == b + 1 = geraIntervalo xs ((a, x):rest)
      | otherwise = geraIntervalo xs ((x, x):(a, b):rest)
  


      data Contacto = Casa Integer
      | Trab Integer
      | Tlm Integer
      | Email String
      deriving (Show)
      type Nome = String
      type Agenda = [(Nome, [Contacto])]


      acrescEmail :: Nome -> String -> Agenda -> Agenda 
      acrescEmail []=[a]
      acrescEmail nome Email Agenda=  case lookup nome agenda of
        Just contatos -> (nome, contatos ++ [Email email]) : filter (\(n, _) -> n /= nome) agenda
        Nothing -> (nome, [Email email]) : agenda


        verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome agenda =
    case lookup nome agenda of
        Just contatos -> Just [email | Email email <- contatos]
        Nothing -> Nothing


        
        consulta :: [Contacto] -> ([Integer], [String])
        consulta contatos =
            let (telefones, emails) = foldr (\cont (tels, mails) -> case cont of
                                                    Casa tel -> (tel:tels, mails)
                                                    Trab tel -> (tel:tels, mails)
                                                    Tlm tel -> (tel:tels, mails)
                                                    Email email -> (tels, email:mails)
                                            ) ([], []) contatos
            in (telefones, emails)

            
            consultaIO :: Agenda -> IO ()
consultaIO agenda = do
    putStrLn "Digite o nome para consultar:"
    nome <- getLine
    case lookup nome agenda of
        Just contatos -> putStrLn $ "Contactos de " ++ nome ++ ": " ++ show contatos
        Nothing -> putStrLn "Esse nome não está na agenda."


        data RTree a = R a [RTree a] deriving (Show, Eq)

paths :: RTree a -> [[a]]
paths (R x []) = [[x]] -- Se for uma folha, retorna uma lista com o elemento atual
paths (R x xs) = map (x:) (concatMap paths xs) -- Mapeia cada caminho possível da raiz até as folhas




unpaths :: Eq a => [[a]] -> RTree a
unpaths [[]] = R undefined [] -- Se a lista estiver vazia, retorna uma árvore vazia
unpaths paths = R (head (head paths)) (map unpaths' (groupByPrefix paths))
  where
    unpaths' :: Eq a => [a] -> RTree a
    unpaths' [x] = R x [] -- Se houver apenas um elemento, é uma folha
    unpaths' (x:xs) = R x [unpaths' xs] -- Caso contrário, é um nó interno com um único subnó
    groupByPrefix :: Eq a => [[a]] -> [[a]]
    groupByPrefix [] = []
    groupByPrefix (x:xs) = (x : takeWhile ((== head x) . head) xs) : groupByPrefix (dropWhile ((== head x) . head) xs)
