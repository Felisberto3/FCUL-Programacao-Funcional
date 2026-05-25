import GHC.IO.Handle (hGetContents)
import GHC.IO.Handle.FD (withFile)
import GHC.IO.IOMode (IOMode (ReadMode))

fromFile :: (Read a) => FilePath -> IO [a]
fromFile filePath =
  withFile filePath ReadMode $
    ( \handle -> do
        contents <- hGetContents handle
        putStr contents
        let resultado = texto contents
        seq (length resultado) (return resultado)
    )
  where
    texto w = map read $ lines w

putOnScreen :: FilePath -> IO ()
putOnScreen filePath = do
  list <- (fromFile filePath :: IO [Int])
  let w = unlines $ map show list
  putStr w
