module Ex9 (Arvore (..)) where

data Arvore a = Folha | No (Arvore a) a (Arvore a)

instance (Show a) => Show (Arvore a) where
  show Folha = "  Folha\n  "
  show (No l x r) = show x ++ "\n  " ++ show l ++ show r

instance Foldable Arvore where
  foldr _ z Folha = z
  foldr f z (No l x r) = foldr f (f x (foldr f z r)) l

length' :: Arvore a -> Int
length' = foldr (\_ acc -> 1 + acc) 0

sum' :: (Num a) => Arvore a -> a
sum' = foldr (+) 0

elem' :: (Eq a) => a -> Arvore a -> Bool
elem' e = foldr (\x acc -> x == e || acc) False

toListR :: Arvore a -> [a]
toListR = foldr (:) []

toListL :: Arvore a -> [a]
toListL = foldl (\acc x -> acc ++ [x]) []
