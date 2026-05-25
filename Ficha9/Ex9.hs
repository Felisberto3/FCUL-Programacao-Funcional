import System.Random

type Carta = Int

type Mao = [Carta]

mao :: Int -> Mao
mao n = take n list
  where
    gen = mkStdGen n
    list = randomRs (1, 52) gen

-- b)
mao' :: Int -> IO Mao
mao' n = do
  gen <- getStdGen
  let list = take n (randoms gen) :: Mao
  return list