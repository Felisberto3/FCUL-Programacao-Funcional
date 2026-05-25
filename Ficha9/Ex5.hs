import Data.List (isPrefixOf)
import GHC.IO.Handle (hGetContents, hPutStr)
import GHC.IO.Handle.FD (withFile)
import GHC.IO.IOMode (IOMode (ReadMode, WriteMode))

filterFiles :: (String -> Bool) -> FilePath -> FilePath -> IO ()
filterFiles f path1 path2 = do
  withFile path1 ReadMode $
    ( \h1 -> do
        contents1 <- hGetContents h1
        withFile path2 WriteMode $
          ( \h2 -> do
              let content = unlines $ filtrarContent f contents1
              hPutStr h2 content
          )
    )
  where
    filtrarContent f w = filter f (texto w)
    texto = lines

-- com writeFile
filterFiles2 :: (String -> Bool) -> FilePath -> FilePath -> IO ()
filterFiles2 f path1 path2 = do
  clue <- readFile path1
  let contentToWrite = unlines $ filtrarContent f clue
  writeFile path2 contentToWrite
  where
    filtrarContent f w = filter f (texto w)
    texto = lines

-- b)
filterPrefix :: String -> FilePath -> FilePath -> IO ()
filterPrefix wordPattern path1 path2 = do
  contents <- readFile path1
  let finalcontent = filterTexto contents
  writeFile path2 finalcontent
  where
    filterTexto w = unlines (filter (wordPattern `isPrefixOf`) $ lines w)

filterPrefix2 :: String -> FilePath -> FilePath -> IO ()
filterPrefix2 wordPattern path1 path2 = do
  withFile path1 ReadMode $
    ( \h1 -> do
        c <- hGetContents h1
        withFile path2 WriteMode $
          ( \h2 -> do
              hPutStr h2 (filterTexto wordPattern c)
          )
    )
  where
    filterTexto wordPattern w = unlines (filter (wordPattern `isPrefixOf`) $ lines w)
