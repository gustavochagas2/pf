idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ []= []
idade x y ((a,b):xs) | x - y >= b = a:idade x y xs
                     | otherwise = idade x y xs
