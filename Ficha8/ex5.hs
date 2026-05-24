guess :: Int -> IO ()
guess n = do
  sortear 0 n 1
  where
    sortear :: Int -> Int -> Int -> IO ()
    sortear i f acc = do
      let v = i + (f - i) `div` 2
      putStr $ show v ++ "?"
      c <- getLine
      case c of
        ">" -> sortear v f (acc + 1)
        "<" -> sortear i v (acc + 1)
        "=" -> putStrLn $ "Sucesso apos " ++ show acc ++ " tentativas "
