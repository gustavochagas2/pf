data ExpInt = Const Int
| Simetrico ExpInt
| Mais ExpInt ExpInt
| Menos ExpInt ExpInt
| Mult ExpInt ExpInt

calcula :: ExpInt -> Int 
calcula (const x)= x
calcula (simetrico x)= (-calcula x)
calcula (mais x y)= x +y
calcula (menos x y) x -y
calcula (mult x y)x *y

infixa :: ExpInt -> String
infixa (const x) = show x
infixa (simetrico x)="-" ++ infixa x
infixa (Mais a b) = '(':infixa a ++ "+" ++ infixa b ++ ")"
infixa (Menos a b) = '(':infixa a ++ "-" ++ infixa b ++ ")"
infixa (Mult a b) = '(':infixa a ++ "*" ++ infixa b ++ ")"


 posfixa :: ExpInt -> String
posfixa (const x) = show x ++ " "
posfixa (simetrico x) = posfixa x ++ "-"
posfixa ( mais a b)= posfixa a ++ posfixa b ++ "+"
posfixa (menos a b) = posfixa a ++ posfixa b ++ "-"
posfixa (mult a b) = posfixa a ++ posfixa b ++ "*"


data RTree a = R a [RTree a]
soma :: Num a => RTree a -> a
soma (R x []) = x
soma (R x xs) = x + sum (map soma xs)

alturaRT :: RTree a -> Int
alturaRT (R x []) = 1
alturaRT (R x l) = 1 + maximum (map alturaRT l)

prune :: Int -> RTree a -> RTree a
prune 0 (R x xs)= r x []
prune y (R x xs) = (R x (prune(y-1)xs))

mirror :: RTree a -> RTree a 
mirror (R x xs) = R x (reverse (map mirror xs))
postorder :: RTree a -> [a]
postorder (R x []) = [x]
postorder (R x xs) = concatMap postorder xs ++ [x]


data BTree a = Empty | Node a (BTree a) (BTree a)
data LTree a = Tip a | Fork (LTree a) (LTree a)
ltSum :: Num a => LTree a -> a
ltSum Tip a = a
ltSum (Fork a b) = ltSum a + ltSum b
listaLT :: LTree a -> [a]
listaLT (Tip x) = [x]
listaLT (Fork left right) = listaLT left ++ listaLT right

ltHeight :: LTree a -> Int
ltHeight (Tip _) = 1
ltHeight (Fork left right) = 1 + max (ltHeight left) (ltHeight right)

data FTree a b = Leaf b | No a (FTree a b) (FTree a b)


splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf x) = (Empty, Tip x)
splitFTree (No a left right) = let
    (leftB, leftL) = splitFTree left
    (rightB, rightL) = splitFTree right
    in (Node a leftB rightB, Fork leftL rightL)


    oinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Tip x) = Just (Leaf x)
joinTrees (Node a leftB rightB) (Fork leftL rightL) = do
    leftFTree <- joinTrees leftB leftL
    rightFTree <- joinTrees rightB rightL
    return (No a leftFTree rightFTree)
joinTrees _ _ = Nothing