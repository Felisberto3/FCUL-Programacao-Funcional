import Control.Monad (replicateM)

main :: IO ()
main = do
  putStrLn "Quantos numeros? "
  i <- readLn :: IO Int
  total <- pegarNumeros i 0
  putStrLn $ "O total e " ++ show total
  where
    pegarNumeros :: Int -> Int -> IO Int
    pegarNumeros 0 acc = return acc
    pegarNumeros c acc = do
      c <- readLn :: IO Int
      pegarNumeros (c - 1) (acc + c)

mainS :: IO ()
mainS = do
  putStrLn "Quantos numeros? "
  c <- readLn :: IO Int
  results <- sequence (replicate c (readLn :: IO Int))

  let total = sum results
  putStrLn $ " O total e " ++ show total