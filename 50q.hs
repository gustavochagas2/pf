

FromTo2 :: Int -> Int ->[Int]
FromTo2 x>y = []
FromTo2 x y = x :FromTo2(x +1) 

fromto :: Int -> Int -> Int -> [Int]

fromto x y z 
    |x > z = []
    |otherwise = x:fromto (x+y-1) y z


   (++) :: [a] -> [a] -> [a]
   (++) [] [a] = [a]
   (++) (h1:t1) (h2:t2) =  h1 t1: (++) h2 t2




   (!!) :: [a] -> Int -> a
   (!!) [] x = _
   (!!) (h:t) x | x== 0 = h
                | x>0 == (!!) t (x-1)

 reverse :: [a] -> [a]    
 reverse [] = []
 reverse (h:t) = t: reverse h            




 take :: Int -> [a] -> [a]
 take 0 [a] = [a]
 take x [] = []
 take x (h:t) | x ==1 = h
              | otherwise = h:take (x-1) t


drop :: Int -> [a] -> [a]
 drop 0 [a] = [a]
  drop x [] = []   
  drop x (h:t) = drop (x-1) t

zip :: [a] -> [b] -> [(a,b)]
zip _ _ = []
zip _ [b]= [b]
zip [a] _= [a]
zip (h1:t1) (h2:t2)=(h1,h2): zip t1 t2

replicate :: Int -> a -> [a]
 replicate 0 x = []
 replicate _ x = []
 replicate n _ = []
  replicate x n = n: replicate (x-1) n

  intersperse :: a -> [a] -> [a]
  intersperse x [] = []
  intersperse _ [a] = [a]
  intersperse x (h:t) =h:x:intersperse x t


  group :: Eq a => [a] -> [[a]]
  group [] = []
  group (x:xs) = (x : takeWhile (== x) xs) : group (dropWhile (== x) xs)

  

  concat :: [[a]] -> [a] 
  concat [[]]= []
  concat (h:t)= h : concat t


  inits :: [a] -> [[a]]
  inits [] = [[]]
  inits (h:t) = [] :[h] ++ inits h t

  tails :: [a] -> [[a]]
  tails [] = [[]]
tails [x] = [[x],[]]

  tails l = l : tail l

  heads :: [[a]] -> [a] 
  heads [] = []
  heads (h:t) = h: heads t 


  total :: [[a]] -> Int
  total [[]] = 0
  total [[x]] = 1
  total l = sum (map length l)

  fun :: [(a, b, c)] -> [(a, c)]
  fun [] = []
  fun [(_, _, _)] = []
  fun ((a, _, c):t) = (a, c) : fun t

  
  cola :: [(String,b,c)] -> String 
  cola [(_,_,_)] = _
  cola ((a,b,c):t) = a ++ cola t


  idade :: Int -> Int -> [(String,Int)] -> [String]
  idade _ _ = []

  idade x y ((a,b):t) =  if  x-b >=y then a : idade x y t
                      else = idade x y t


 isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | n == 2 = True
  | otherwise = isPrime' 2
  where
    isPrime' m
      | m * m > n = True
      | n `mod` m == 0 = False
      | otherwise = isPrime' (m + 1)

 prefix :: Eq a => [a]-> [a] -> Bool
prefix [a][]= True
prefix [][a]= True
prefix (h1:t1) (h2:t2) | h1 == h2 = prefix t1 t2
                       | h1/=h2 = False
                      | otherwise = True

sufix :: Eq a => [a]-> [a] -> Bool 
sufix [a][] = False
sufix [][a] = False
sufix (h1:t1) (h2:t2) | t1 == t2 = True
                      | t1/= t2 = sufix t1 ( tail t2)
                      | otherwise = False
                      
isSubsequenceOf :: Eq a =>[a] -> [a] -> Bool 
isSubsequenceOf [a][]= True
isSubsequenceOf [][a]= False
isSubsequenceOf (h1:t1)(h2:t2) | h1 = h2 = isSubsequenceOf t1 t2
                               |h1 /= h2 = isSubsequenceOf (h1:t1) t2
                               
  elemIndices :: Eq a => a -> [a] -> [Int]
  elemIndices _ [] = []
  elemIndicies x (h:t)| x == h = 0: map (+1) (elemIndices x t)
                      | otherwise =map (+1) (elemIndices x t)



 nub :: Eq a => [a] -> [a]
 nub [] = []
 nub (h:t) |elem h t = nub t
           | otherwise = h: nub t
           

  delete :: Eq a => a -> [a]-> [a]  
   delete _ [x] = [x]
   delete n []= []
   delete n (h:t) |h == n = t
                  | otherwise = h: delete n t   
                  
                  
 (\\):: Eq a => [a] -> [a] -> [a]
 (\\) [] [] = []
 (\\) (h1:t1)[] = (h1:t1)
 (\\) (h1:t1) (h2:t2) | h1== h2 = (\\) t1 t2 
              | otherwise = h1: (\\) t1 (h2:t2)
                      
 union :: Eq a => [a] -> [a]  -> [a]
 union [][]  = []
 union (h1:t1)[]= (h1:t1)
 union [] (h2:t2) = (h2:t2)
 union (h1:t1) (h2:t2) | elem h (h2:t2) = union (h2:t2) t
                       | otherwise = union((h2:t2)++[h]) t

 intersect :: Eq a => [a] -> [a] -> [a]
intersect [][]= []
intersect [] _ = []
intersect _ [] = []
intersect (h:t) x| elem h x = h:intersect t x
                 |otherwise = intersect t y


 insert :: Ord a => a -> [a] -> [a]
 insert _ [x] = [x]~
 insert n [] = [n]
 insert n (h:t)| n>=h = n:h:t
               |otherwise = h:insert n t


 unwords:: [String] -> String
 unwords []= ""
unwords (h:t)  h ++ " " ++ unwords t    

unlines :: [String] -> String    
unlines []= ""
unlines (h:t)= h ++"\n" ++ unlines t


pMaior:: Ord a => [a] -> Int
pMaior [_]= 0

pMaior (x:xs) = pMA (x, 0 , 1) xs

		
pMA:: Ord a => (a,Int,Int) -> [a] -> Int
pMA (m,pm,pa) [] = pm
pMA (m,pm,pa) (h:t) | h > m = pMA (h,pa,pa+1) t
					| otherwise = pMA (m,pm,pa+1) t


 lookup :: Eq a => a -> [(a,b)] -> Maybe b
 lookup _[]= nothing
 lookup x ((a,b):ys) | x == a = Just b
					 | otherwise = lookup' x ys

 preCrescente :: Ord a => [a] -> [a] 
 preCrescente []=[]
 preCrescente (h:t) | h< head t= h: preCrescente t
                    | otherwise = x:[]

 iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:xs) = insert x (iSort xs)


menor :: String -> String -> Bool
menor [][]= False
menor []_ = True
menor_[] = False
menor (h1:t1) (h2:t2) = menor  t1 t2




elemMSet :: Eq a => a -> [(a,Int)] -> Bool 
elemMSet _[]= False
elemMSet x ((a,b):t) | x == a = True
                     | otherwise = elemMSet x t



converteMSet :: [(a,Int)] -> [a] 
converteMSet [] = []
converteMSet ((a,b):ys) | b /= 0 = replicate' b a ++ converteMSet ys
						| otherwise = converteMSet ys


    insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
 insereMSet x ((a,b):ys) | x == a = (a,b+1):ys
                         | otherwise = (a,b): insereMSet x ys


removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet x ((a,b):t) | b > 1 && x == a = (a,(b-1)):t
                       |  b==1  && x==a = t
                       | otherwise= (a,b):RemoveMSet a t



  constroiMSet :: Ord a => [a] -> [(a,Int)]
 constroiMSet [] = []
 constroiMSet (x:xs) = insereMSet x (constroiMSet xs)



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