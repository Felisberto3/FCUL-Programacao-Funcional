-- -- ENTRADA
-- getLine    :: IO String           -- lê linha inteira
-- getChar    :: IO Char             -- lê um caractere
-- getContents :: IO String          -- lê tudo até EOF (lazy)
-- interact   :: (String->String) -> IO ()  -- transforma stdin em stdout

-- -- SAÍDA
-- putChar    :: Char -> IO ()
-- putStr     :: String -> IO ()
-- putStrLn   :: String -> IO ()
-- print      :: Show a => a -> IO () -- = putStrLn . show

-- -- FICHEIROS
-- readFile   :: FilePath -> IO String
-- writeFile  :: FilePath -> String -> IO ()
-- appendFile :: FilePath -> String -> IO ()

-- -- CONTROLO
-- return     :: a -> IO a           -- embrulha valor puro em IO
--            -- NÃO termina o programa! só "levanta" um valor

-- -- Exemplo: return não termina!
-- main = do
--   return "ignorado"   -- executa mas o resultado é deitado fora
--   putStrLn "isto imprime na mesma"