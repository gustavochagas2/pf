 randomIO :: Random a => IO a
 randomRIO :: Random a => (a,a) -> IO a


 bingo :: IO () 
 bingo = bingoLoop []

 bingoLoop :: [Int] -> IO ()
 bingoLoop nums
     | length nums == 90 = putStrLn "Todos os números foram sorteados."
     | otherwise = do
         _ <- getChar -- Aguarda que o usuário pressione qualquer tecla para gerar um número
         num <- randomRIO (1, 90)
         if num `elem` nums
             then bingoLoop nums -- Se o número já foi sorteado, continua para o próximo
             else do
                 putStrLn $ "Número sorteado: " ++ show num
                 bingoLoop (num : nums)



  mastermind :: IO ()
 mastermind do=
    x <- ramdomRIO (0,9)
    y<- ramdomRIO(0,9)
    w<- ramdomRIO (0,9)
    z<- ramdomRIO(0,9)
    let secret = [x,y,w,z]
    play secret

    play:: [Int] -> IO () 
    play secret = do
        putStrLn "Digite uma sequência de 4 dígitos (0-9): "
        input <- getLine
        if lenght input /=4 then  putStrLn "Digite uma sequência de 4 dígitos (0-9): "

        else do
            let guess = map (read . return) input
            feedback = checkGuess secret guess
        putStrLn $ "Números corretos na posição correta: " ++ show (fst feedback)
        putStrLn $ "Números corretos na posição errada: " ++ show (snd feedback)
        if fst feedback == 4
            then putStrLn "Parabéns! Você acertou!"
            else play secret

checkGuess :: [Int] -> [Int] -> (Int, Int)
checkGuess secret guess = (correctPos, correctDigits)
where
    correctPos = length $ filter (\(x, y) -> x == y) (zip secret guess)
    correctDigits = length (intersect secret guess) - correctPos


    data Aposta = Ap [Int] (Int,Int)
    valida :: Aposta -> Bool
    valida (Ap nums (est1, est2)) =
        length nums == 5 &&
        length (nub nums) == 5 &&
        all (\x -> x `elem` [1..50]) nums &&
        est1 `elem` [1..9] &&
        est2 `elem` [1..9] &&
        est1 /= est2


        comuns :: Aposta -> Aposta -> (Int, Int)
comuns (Ap nums1 (est1_1, est1_2)) (Ap nums2 (est2_1, est2_2)) =
    let commonNums = intersect nums1 nums2
        commonEst = length (intersect [est1_1, est1_2] [est2_1, est2_2])
    in (length commonNums, commonEst)

    instance Eq Aposta where
    (Ap nums1 est1) == (Ap nums2 est2) = nums1 == nums2 && est1 == est2


    premio :: Aposta -> Aposta -> Maybe Int
premio aposta chave
    | aposta == chave = Just 1
    | otherwise =
        case (comuns aposta chave) of
            (5, 2) -> Just 7
            (5, 1) -> Just 8
            (5, 0) -> Just 9
            (4, 2) -> Just 10
            (4, 1) -> Just 11
            (4, 0) -> Just 12
            (3, 2) -> Just 13
            _ -> Nothing


            leAposta :: IO Aposta
            leAposta = do
                putStrLn "Digite 5 números (entre 1 e 50) separados por espaços:"
                numsInput <- getLine
                let nums = map read $ words numsInput :: [Int]
            
                putStrLn "Digite 2 estrelas (entre 1 e 9) separadas por espaços:"
                estInput <- getLine
                let [est1, est2] = map read $ words estInput :: [Int]
            
                let aposta = Ap nums (est1, est2)
                if valida aposta
                    then return aposta
                    else do
                        putStrLn "A aposta não é válida. Tente novamente."
                        leAposta
            
geraChave :: IO Aposta
geraChave = do
    gen <- getStdGen
    let nums = take 5 (randomRs (1, 50) gen)
        est = take 2 (randomRs (1, 9) gen)
    return $ Ap nums (head est, last est)
    joga :: Aposta -> IO ()
    joga chave = do
        putStrLn "Insira sua aposta:"
        aposta <- leAposta
        case premio aposta chave of
            Just prem -> putStrLn $ "Seu prêmio é: " ++ show prem
            Nothing -> putStrLn "Infelizmente, você não ganhou nada."
    