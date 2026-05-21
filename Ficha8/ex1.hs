import Data.List hiding (findIndex)

primos :: [Integer]
primos = sieve [2 ..]
  where
    sieve (p : xs) =
      p : sieve [x | x <- xs, x `mod` p > 0]

escrevePrimos :: [Int] -> IO ()
escrevePrimos ordens = putStr (unlines (map formataMensagem ordens))
  where
    formataMensagem n = show n ++ "º numero primo " ++ show (primos !! n)