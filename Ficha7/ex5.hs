import Data.List hiding (singleton)
import Test.QuickCheck

data Arvore a = Folha | No (Arvore a) a (Arvore a) deriving (Show, Eq)

-- a)
empty :: Arvore a
empty = Folha

-- b)
singleton :: a -> Arvore a
singleton a = No empty a empty

-- c)
size :: Arvore a -> Int
size Folha = 0
size (No left x right) = 1 + size left + size right

-- d)
depth :: Arvore a -> Int
depth Folha = 0
depth (No left x right) = 1 + max (depth left) (depth right)

-- e)
isPerfect :: Arvore a -> Bool
isPerfect Folha = True
isPerfect (No left x right) = size left == size right && isPerfect left && isPerfect right

isPerfect' :: (Eq a) => Arvore a -> Bool
isPerfect' Folha = True
isPerfect' (No left x right) = verify && isPerfect' left && isPerfect' right
  where
    verify = not condition
    condition = left == Folha && right /= Folha || left /= Folha && right == Folha