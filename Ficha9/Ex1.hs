import Data.Char (toUpper)
import GHC.IO.Handle (hPutStr)
import GHC.IO.Handle.FD (withFile)
import GHC.IO.IOMode

toFile :: (Show a) => FilePath -> [a] -> IO ()
toFile filePath list = do
  withFile filePath WriteMode $
    ( \handle -> do
        let w = unlines (map show list)
        hPutStr handle w
    )

toFile2 :: (Show a) => FilePath -> [a] -> IO ()
toFile2 filePath list = do
  let conteudo = unlines $ map transformar list
  appendFile filePath conteudo
  where
    maiuscula = map toUpper
    transformar w = maiuscula (show w)
