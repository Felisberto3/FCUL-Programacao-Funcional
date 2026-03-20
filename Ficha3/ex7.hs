-- 7. Escreva uma função que devolva a abreviatura do ordinal em inglês
-- para um número cardinal positivo. Ex. ordinalPrefix 31 deverá
-- devolver "31st" e ordinalPrefix 7 deverá devolver "7th".
ordinalPrefix :: (Show a) => a -> [Char]
ordinalPrefix a
  | last (show a) == '1' = show a ++ "st"
  | last (show a) == '2' = show a ++ "nd"
  | last (show a) == '3' = show a ++ "rd"
  | otherwise = show a ++ "th"

-- outra forma
ordinalPrefix2 :: (Show a, Integral a) => a -> [Char]
ordinalPrefix2 a
  | resto == 1 = show a ++ "st"
  | resto == 2 = show a ++ "nd"
  | resto == 3 = show a ++ "rd"
  | otherwise = show a ++ "th"
  where
    resto = a `mod` 10

-- mas nao faco isso na vida real!!!kkkkkkkkk, só estava ver se é possível!
ordinalPrefix3 :: Int -> String
ordinalPrefix3 a =
  let resto = a `mod` 10
   in if resto == 1
        then show a ++ "st"
        else
          if resto == 2
            then show a ++ "nd"
            else
              if resto == 3
                then show a ++ "rd"
                else show a ++ "th"