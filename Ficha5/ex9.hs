-- a)
meuIf :: (a -> Bool) -> (a -> b) -> (a -> b) -> [a] -> [b]
meuIf _ _ _ [] = []
meuIf f g h (x : xs)
  | f x = g x : meuIf f g h xs
  | otherwise = h x : meuIf f g h xs

-- b)
meuif2 :: (a -> Bool) -> (a -> b) -> (a -> b) -> [a] -> [b]
meuif2 f g h list = [if f x then g x else h x | x <- list]

-- c)
meuif3 :: (a -> Bool) -> (a -> b) -> (a -> b) -> [a] -> [b]
meuif3 f g h = map (\x -> if f x then g x else h x)
