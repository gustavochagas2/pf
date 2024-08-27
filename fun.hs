-- Pergunta 1 --
enumFromTo':: Int -> Int -> [Int]
enumFromTo' x y | x > y = []
				| otherwise = x:enumFromTo' (x+1) y

-- Pergunta 2 --
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' x y z | (x > y && y > z) || (x == y && y < z) || (x > y && x==z) = []
					  | otherwise = x: enumFromThenTo' y (y+y-x) z

-- Pergunta 3 -- 
junta2Listas:: [a] -> [a] -> [a]
junta2Listas x [] = x
junta2Listas [] y = y
junta2Listas (x:xs) (y:ys) = x: junta2Listas xs (y:ys)

-- Pergunta 4 --
encontraLista:: [a] -> Int -> a
encontraLista (h:_) 0 = h 
encontraLista (x:xs) y | y == 0 = x 
					   | otherwise = encontraLista xs (y-1)

-- Pergunta 5 --
reverse':: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


-- Pergunta 6 --
take':: Int -> [a] -> [a]
take' _ [] = []
take' x (y:ys) | x == 0 = []
			   | otherwise = y:take' (x-1) ys

-- Pergunta 7 -- 
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' x (y:ys) | x == 0 = (y:ys)
			   | otherwise = drop' (x-1) ys

-- Pergunta 8 --
zip':: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip'  [] _  = []
zip'  _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

-- Pergunta 9 --
replicate' :: Int -> a -> [a]
replicate' 0 a = []
replicate' x y | x == 0 = []
			  | otherwise = y:replicate' (x-1) y


-- Pergunta 10 --
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse x (y:[]) = [y]
intersperse x (y:ys) = y:x:intersperse x ys

-- Pergunta 11 --
group:: Eq a => [a] -> [[a]]
group [] = []
group [h] = [[h]]
group (h:t) = let ((x:xs):ys) = group t
                     in if h == x then (h:x:xs):ys
                        else [h] : (x:xs):ys
-- Pergunta 12 --
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- Pergunta 13 --
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l] 

-- Pergunta 14 --
tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs): tails xs


-- Pegunta 15 --
heads :: [[a]] -> [a]
heads [] = []
heads  ([]:xs) = heads xs 
heads ((y:ys):xs) = y: heads xs

-- Pergunta 16 -- 
total :: [[a]] -> Int
total [] = 0
total ([]:t) = 0 + total t
total (x:xs) = length x + total xs

-- Pergunta 17 -- 
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,b,c):xs) = (a,c):fun xs

-- Pergunta 18 --
cola :: [(String,b,c)] -> String
cola [] = []
cola ((a,b,c):xs) = a ++ cola xs

-- Pergunta 19 --
idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ []= []
idade x y ((a,b):xs) | x - y >= b = a:idade x y xs
					 | otherwise = idade x y xs

-- Pergunta 20 --
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n 1 = [1]
powerEnumFrom n m
    | m > 1 = powerEnumFrom n (m - 1) ++ [n^(m-1)]
    | otherwise = []

-- Pergunta 21 --

testa :: Int -> Int -> Bool
testa n m | m*m > n = True
		  | mod n m == 0 = False 
		  | otherwise = testa n (m+1)

isPrime :: Int -> Bool
isPrime x | x >= 2 = testa x 2
		  | otherwise = False

 -- Pergunta 22 --
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' _ [] = False
isPrefixOf' [] _ = True
isPrefixOf' (x:xs) (y:ys) = x == y && isPrefixOf' xs ys

-- Pergunta 23 --
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = False
isSuffixOf _ [] = False
isSuffixOf l1 l2
    |l1 == l2 = True
    |l1 /= l2 = isSuffixOf l1 (tail l2)
    |otherwise = False

-- Pergunta 24 --
isSubsequenceOf:: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] [] = True
isSubsequenceOf [] _ = False
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys)   | x == y  = isSubsequenceOf xs ys
							 	| x /= y  = isSubsequenceOf (x:xs) ys

-- Pergunta 25 --
elemIndices:: Eq a => a -> [a] -> [Int]
elemIndices x l = eIA 0 x l

eIA:: Eq a => Int -> a -> [a] -> [Int]
eIA p x [] = []
eIA p x (h:t) | x == h = p: eIA p+1 x t
			  | otherwise = eIA (p+1) x t

-- Pergunta 26 -- 
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) | elem x xs = nub xs
		   | otherwise = x:nub xs

-- Pergunta 27 --
delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (y:ys) | x == y = ys
				| otherwise = y:delete x ys

-- Pergunta 28 --
encontra:: Eq a => [a] -> [a]-> [a] 
encontra [] _ = []
encontra (x:xs) [] = (x:xs) 
encontra (x:xs) (y:ys) | x == y = encontra xs ys
				 	   | otherwise = x:encontra xs (y:ys)

-- Pergunta 29 --
union :: Eq a => [a] -> [a]-> [a]
union [] [] = []
union (x:xs) [] = (x:xs)
union [] (y:ys) = (y:ys)
union (y:ys) (x:xs) | elem x (y:ys) = union (y:ys) xs
				 	| otherwise = union ((y:ys) ++ [x]) xs

-- Pergunta 30 --
intersect :: Eq a => [a] -> [a] -> [a] 
intersect [] [] = []
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) y | elem x y = x:intersect xs y
				   | otherwise = intersect xs y

-- Pergunta 31 --
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x < y = x:y:ys
				| otherwise = y:insert x ys

-- Pergunta 32 --
unwords':: [String] -> String
unwords' [] = []
unwords' (x:xs) = x ++ " " ++ unwords' xs

-- Pergunta 33 -- 
unlines' :: [String] -> String
unlines' [] = []
unlines' (x:[]) = x ++ "\n"
unlines' (x:xs) = x ++ "\n" ++ unlines' xs

-- Pergunta 34 -- 
pMaior:: Ord a => [a] -> Int
pMaior [_] = 0 -- Retorna 0 se for só 1 elemento
pMaior (x:xs) = pMA (x, 0 , 1) xs

		
pMA:: Ord a => (a,Int,Int) -> [a] -> Int
pMA (m,pm,pa) [] = pm
pMA (m,pm,pa) (h:t) | h > m = pMA (h,pa,pa+1) t
					| otherwise = pMA (m,pm,pa+1) t



-- Pergunta 35 --
lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' _ [] = Nothing 
lookup' x ((a,b):ys) | x == a = Just b
					 | otherwise = lookup' x ys

-- Pergunta 36 -- 
preCrescente :: Ord a => [a] -> [a] 
preCrescente [] = []
preCrescente (x:xs) | x < head xs = x:preCrescente xs
					| otherwise = x:[]

-- Pergunta 37 -- 
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:xs) = insert x (iSort xs)

-- Pergunta 38 --
menor :: String -> String -> Bool
menor [] [] = False
menor _ [] = False
menor [] _ = True
menor (x:xs) (y:ys) = menor xs ys

-- Pergunta 39 --
elemMSet :: Eq a => a -> [(a,Int)] -> Bool 
elemMSet _ [] = False
elemMSet x ((a,b):ys) | x == a = True
					  | otherwise = elemMSet x ys

-- Pergunta 40 -- 
converteMSet :: [(a,Int)] -> [a] 
converteMSet [] = []
converteMSet ((a,b):ys) | b /= 0 = replicate' b a ++ converteMSet ys
						| otherwise = converteMSet ys

-- Pergunta 41 --
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet x ((a,b):ys) | x == a = (a,b+1):ys
						| otherwise = (a,b): insereMSet x ys

-- Pergunta 42 --
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] 
removeMSet _ [] = []
removeMSet x ((a,b):ys) | x == a = ys
						| otherwise = (a,b):removeMSet x ys

-- Pergunta 43 --
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (x:xs) = insereMSet x (constroiMSet xs)

-- Pergunta 44 --
partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' ((Left a):t) = (a:as,bs)
    where (as,bs) = partitionEithers' t
partitionEithers' ((Right b):t) = (as,b:bs)
    where (as,bs) = partitionEithers' t  

-- Pergunta 45 --
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:t) = catMaybes t
catMaybes (Just x:t) = x:catMaybes t

-- Pergunta 46 -- 
data Movimento = Norte | Sul | Este | Oeste deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (a,b) (c,d) | a < c = Este: caminho (a,b) (c-1,d)
					| a > c = Oeste: caminho (a,b) (c+1,d)
					| b < d = Norte: caminho (a,b) (c,d-1)
					| b > d = Sul: caminho (a,b) (c,d+1)
					| otherwise = []
-- Pergunta 47 --
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



-- Pergunta 48 -- 
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int  
contaQuadrados [] = 0
contaQuadrados ((Rect (a,b) (c,d)):xs) | abs (c-a) == abs (b-d) = 1+contaQuadrados xs
									   | otherwise = 0 + contaQuadrados xs


-- Pergunta 49 --
areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (a,b) (c,d)):xs) = abs(c-a)*abs(b-d) + areaTotal xs

-- Pergunta 50 --
data Equipamento = Bom | Razoavel | Avariado deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (Bom:t) = 1+ naoReparar t
naoReparar (Razoavel:t) = 1 + naoReparar t
naoReparar (Avariado:t) = naoReparar t