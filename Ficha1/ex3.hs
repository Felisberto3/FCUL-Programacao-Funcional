addDigit :: (Num a, Ord a) => a -> a -> a
addDigit x y = if x >= 0 then x * 10 + y else x * 10 - y

addDigit2 :: (Ord a, Num a) => a -> a -> a
addDigit2 x y
  | x >= 0 = x * 10 + y
  | otherwise = x * 10 - y