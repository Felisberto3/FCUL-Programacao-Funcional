poly :: Int -> [Int] -> Int
poly x list = snd $ foldl (\(e, v) n -> (e + 1, v + n * (x ^ e))) (0, 0) list

poly2 :: Int -> [Int] -> Int
poly2 x list = snd $ go
  where
    go = foldr (\n (e, v) -> (e - 1, v + n * x ^ e)) (length', 0) list
    length' = length list - 1