import Control.Monad (when)
import Data.Char (toUpper)
import GHC.IO.Handle (BufferMode (LineBuffering, NoBuffering), hSetBuffering)
import GHC.IO.Handle.FD (stdin, stdout)

isPolindromo :: IO ()
isPolindromo = do
  putStr "Escreva Uma Frase: "
  c <- getLine
  if ePalindromo c then putStrLn "Sim " else putStrLn "Nao"

ePalindromo :: String -> Bool
ePalindromo xs = xs == reverse xs

isPolindromo2 :: IO ()
isPolindromo2 = do
  putStrLn "Escreve uma frase"
  c <- getLine
  when (c /= "\n") $ do
    if ePalindromo c
      then putStrLn "Sim"
      else do
        putStrLn "Nao"
    isPolindromo2

withInteract :: IO ()
withInteract = do
  -- Estas duas linhas forçam o terminal a mostrar o resultado INSTANTANEAMENTE
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  interact f
  where
    f texto = unlines $ map resposta (lines texto)
    resposta palavra = if ePalindromo palavra then "Sim" else "Nao"
