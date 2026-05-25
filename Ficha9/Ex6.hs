import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let path1 = head args
  content <- readFile path1

  let linhas = lines content
  let palavras = map words linhas
  let numeroLinhas = length linhas
  let numeroPalavras = foldl (\acc n -> length n + acc) 0 palavras
  putStrLn $ "Numero de Linhas : " ++ show numeroLinhas
  putStrLn $ "Numero de Palavras : " ++ show numeroPalavras
  putStrLn $ "Numero de caracteres : " ++ show (length content)
  putStrLn $ "Nome do Ficheiro : " ++ path1
