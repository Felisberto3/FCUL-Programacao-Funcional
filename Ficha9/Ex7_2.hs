import System.Environment (getArgs)

main :: IO ()
main = do
  [filePath] <- getArgs
  content <- readFile filePath
  let linhas = lines content
  let linhasEnumeradas = zip [1 ..] linhas
  mapM_ (\(i, l) -> putStrLn (show i ++ "-" ++ l)) linhasEnumeradas