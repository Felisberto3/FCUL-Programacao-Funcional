import GHC.IO.Handle (hGetContents)
import GHC.IO.Handle.FD (withFile)
import GHC.IO.IOMode (IOMode (ReadMode))

sumInts :: FilePath -> IO Int
sumInts filePath =
  withFile filePath ReadMode $
    ( \handle -> do
        contents <- hGetContents handle
        let numeros = map read (lines contents)
        let sumAll = sum numeros
        -- putStr $ "Valor Total " ++ show sumAll
        seq sumAll (return sumAll)
    )