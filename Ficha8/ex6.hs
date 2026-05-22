import System.IO

pedeSegredo :: IO String
pedeSegredo = do
  hSetEcho stdin False
  palavra <- getLine
  hSetEcho stdin True
  putStrLn (replicate (length palavra) '-')
  return palavra

tentativa :: Int -> String -> String -> IO ()
tentativa i partianal full = do
  putStrLn $ "Tentativa " ++ show (i + 1) ++ "º: "
  putStrLn partianal
  t <- getChar
  putStrLn ""
  case jogar t partianal full of
    Nothing -> putStrLn "Acertou"
    Just palavras -> tentativa (i + 1) palavras full

jogar :: Char -> String -> String -> Maybe String
jogar t partialWord fullWord
  | verify == fullWord = Nothing
  | otherwise = Just verify
  where
    verify = zipWith (\p f -> if t == f then f else p) partialWord fullWord

main :: IO ()
main = do
  putStrLn "Primeiro Jogador, pense numa palavra:"
  full <- pedeSegredo
  let partional = replicate (length full) '-'
  tentativa 0 partional full
