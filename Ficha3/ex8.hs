import Data.Char (toUpper)

leetSpeak :: String -> String
leetSpeak [] = []
leetSpeak (x : xs) = l : leetSpeak xs
  where
    l
      | x == 'a' = '4'
      | x == 'i' = '1'
      | x == 't' = '7'
      | x == 'o' = '0'
      | x == 's' = '5'
      | x == 'e' = '3'
      | otherwise = toUpper x
