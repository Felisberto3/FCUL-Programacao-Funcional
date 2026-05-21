imprimePar :: Int -> IO ()
imprimePar x
  | even x = putStrLn "Par"
  | otherwise = putStrLn "Impar"

mostrarParidadeR :: [Int] -> IO ()
mostrarParidadeR [] = return ()
mostrarParidadeR (x : xs) = imprimePar x >> mostrarParidadeR xs

mostrarParidadeR2 :: [Int] -> IO ()
mostrarParidadeR2 [] = return ()
mostrarParidadeR2 (x : xs) = do
  imprimePar x
  mostrarParidadeR2 xs

mostrarParidade :: [Int] -> IO ()
mostrarParidade = mapM_ imprimePar
