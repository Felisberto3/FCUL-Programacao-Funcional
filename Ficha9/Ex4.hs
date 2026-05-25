import GHC.IO.Handle (hGetContents)
import GHC.IO.Handle.FD (withFile)
import GHC.IO.IOMode (IOMode (ReadMode))

mergeFiles :: (Read a, Ord a) => FilePath -> FilePath -> IO [a]
mergeFiles fp1 fp2 =
  withFile
    fp1
    ReadMode
    ( \h1 -> do
        contents1 <- hGetContents h1
        withFile
          fp2
          ReadMode
          ( \h2 -> do
              contents2 <- hGetContents h2
              let list1 = mapToList contents1
              let list2 = mapToList contents2
              let result = merge list1 list2

              seq (length result) (return result)
          )
    )
  where
    mapToList texto = map read $ lines texto

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x > y = y : merge (x : xs) ys
  | x < y = x : merge xs (y : ys)
  | otherwise = x : merge xs ys