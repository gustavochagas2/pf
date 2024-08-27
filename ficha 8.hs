data Frac = F Integer Integer
normaliza :: Frac -> Frac
normaliza F 0 y =F 0 1 
normaliza (f x y)| y=0 == error
|y<0 =F((-x)div(mdc x y)(y div(mdc x y)))
|otherwise = F(x div (mdx x y)) (b div (mdc x y))

mdc :: Integer -> Integer -> Integer
mdc x 0 = x
mdc x y = mdc b (a mod b)

class Eq a where
    (F a b) == (F c d) = a * d == b * c
    instance Ord Frac where
        compare (F a b) (F c d) = compare (a * d) (b * c)
instance Show Frac where
            show (F a b) = "(" ++ show a ++ "/" ++ show b ++ ")"    
instance Num Frac where
    (F a b) + (F c d) = normaliza (F (a * d + b * c) (b * d))
    (F a b) * (F c d) = normaliza (F (a * c) (b * d))
    negate (F a b) = F (-a) b
    abs (F a b) = F (abs a) (abs b)
    signum (F a b) = if a * b >= 0 then 1 else -1
    fromInteger x = F x 1 

maioresQueDobro :: Frac -> [Frac] -> [Frac]
maioresQueDobro f l = filter (> (f * 2)) l



data Exp a = Const a
| Simetrico (Exp a)
| Mais (Exp a) (Exp a)
| Menos (Exp a) (Exp a)
| Mult (Exp a) (Exp a)
instance (Eq a) => Eq (Exp a) where
    (Const x) == (Const y) = x == y
    (Simetrico e1) == (Simetrico e2) = e1 == e2
    (Mais e1 e2) == (Mais e3 e4) = e1 == e3 && e2 == e4
    (Menos e1 e2) == (Menos e3 e4) = e1 == e3 && e2 == e4
    (Mult e1 e2) == (Mult e3 e4) = e1 == e3 && e2 == e4
    _ == _ = False
instance (Show a) => Show (Exp a) where
    show (Const x) = show x
    show (Simetrico e) = "(-" ++ show e ++ ")"
    show (Mais e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (Menos e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
    show (Mult e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"


    instance (Num a) => Num (Exp a) where
    (+) = Mais
    (-) = Menos
    (*) = Mult
    negate = Simetrico
    fromInteger x = Const (fromInteger x)
    abs = undefined
    signum = undefined


    data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extracto = Ext Float [(Data, String, Movimento)]

instance Ord Data where
    compare (D d1 m1 a1)  (D d2 m2 a2)
    |a1 < a2 = True
    | a1 == a2 && m1 < m2 = True
    | a1 == a2 && m1 == m2 && d1 < d2 = True
    | otherwise = False

    instance Show Data where
    Show( D d m a)= Show d ++ "/"++ Show m ++ "/" ++ Show a

    ordena :: Extracto -> Extracto
    ordena (Ext saldo movimentos) = Ext saldo (sortBy (\(d1, _, _) (d2, _, _) -> compare d1 d2) movimentos)

    ordena :: Extracto -> Extracto
ordena (Ext saldo movimentos) = Ext saldo (ordenaMovimentos movimentos)
    where
        ordenaMovimentos :: [(Data, String, Movimento)] -> [(Data, String, Movimento)]
        ordenaMovimentos [] = []
        ordenaMovimentos (x:xs) = insereOrdenado x (ordenaMovimentos xs)

        insereOrdenado :: (Data, String, Movimento) -> [(Data, String, Movimento)] -> [(Data, String, Movimento)]
        insereOrdenado x [] = [x]
        insereOrdenado x@(d1, _, _) (y@(d2, _, _):ys)
            | d1 <= d2 = x : y : ys
            | otherwise = y : insereOrdenado x ys


            
            instance Show Extracto where
                show (Ext saldo movimentos) =
                    "Saldo anterior: " ++ show saldo ++ "\n" ++
                    replicate 40 '-' ++ "\n" ++
                    "Data\tDescricao\tCredito\tDebito\n" ++
                    replicate 40 '-' ++ "\n" ++
                    concatMap (\(data', desc, mov) ->
                        show data' ++ "\t" ++ desc ++ "\t" ++
                        case mov of
                            Credito f -> show f ++ "\t-\n"
                            Debito f -> "-\t" ++ show f ++ "\n"
                    ) movimentos ++
                    replicate 40 '-' ++ "\n" ++
                    "Saldo atual: " ++ show (saldo + sumMovimentos movimentos)
                    where
                        sumMovimentos = foldr (\(_, _, mov) acc ->
                            case mov of
                                Credito f -> acc + f
                                Debito f -> acc - f
                            ) 0
            

