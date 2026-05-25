import System.Environment (getArgs)

main :: IO ()
main = do
  [filePath] <- getArgs
  content <- readFile filePath
  let linhas = lines content
  sequence_ (map putStrLn (linhasComNumeros linhas))
  where
    linhasComNumeros :: [String] -> [String]
    linhasComNumeros linhas = zipWith (\c l -> show c ++ "-" ++ l) [1 ..] linhas