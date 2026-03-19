length1 :: (Num a1) => [a2] -> a1
length1 [] = 0
length1 (x : xs) = 1 + length1 xs