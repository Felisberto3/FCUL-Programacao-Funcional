import System.Environment (getArgs)

main :: IO ()
main = do
  getContent <- getArgs
  let [filePath] = getContent
  content <- readFile filePath
  let linhas = lines content
  putOnScreen linhas 0
  where
    putOnScreen [] _ = return ()
    putOnScreen (l : ls) i = putStrLn (show (i + 1) ++ "-" ++ l) >> putOnScreen ls (i + 1)