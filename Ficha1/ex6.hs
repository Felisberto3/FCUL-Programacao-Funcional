prefix :: (Ord a) => [a] -> [a] -> Bool
prefix a b = take (length a) b == a

prefix' :: (Eq a) => [a] -> [a] -> Bool
prefix' _ [] = False
prefix' [] _ = True
prefix' (a : as) (b : bs) = a == b && prefix' as bs

-- b)
suffix :: String -> String -> Bool
suffix a b = drop (length b - length a) b == a

-- outra forma
suffix1 a b = prefix (reverse a) (reverse b)

-- outra forma
suffix' a b = suffix'' (reverse a) (reverse b)

suffix'' _ [] = False
suffix'' [] _ = True
suffix'' (a : as) (b : bs) = a == b && suffix'' as bs

-- c
isPreffixOrSuffix :: [Char] -> [Char] -> Bool
isPreffixOrSuffix a b = prefix a b || suffix a b