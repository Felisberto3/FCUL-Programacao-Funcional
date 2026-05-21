module Ex5 (empty, Arvore (..)) where

import Data.List hiding (elem, insert, singleton)
import Test.QuickCheck
import Prelude hiding (elem)

data Arvore a = Folha | No (Arvore a) a (Arvore a) deriving (Show)

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

isPerfect' :: Arvore a -> Bool
isPerfect' no = fst $ checkOut no
  where
    checkOut :: Arvore a -> (Bool, Int)
    checkOut Folha = (True, 0)
    checkOut (No left _ right) =
      let (isLPerfect, tamanhoL) = checkOut left
          (isRPerfect, tamanhoR) = checkOut right

          tamanho = 1 + tamanhoL + tamanhoR
          perfect = isLPerfect && isRPerfect && tamanhoL == tamanhoR
       in (perfect, tamanho)

-- f)
insert :: (Ord a) => a -> Arvore a -> Arvore a
insert el Folha = No Folha el Folha
insert el (No left x right)
  | el > x = No left x (insert el right)
  | otherwise = No (insert el left) x right

-- g)
fromList :: (Ord a) => [a] -> Arvore a
fromList = foldl (\acc n -> insert n acc) Folha

-- h)
fromList2 :: (Ord a) => [a] -> Arvore a
fromList2 list = aux list Folha
  where
    aux [] no = no
    aux (x : xs) no = aux xs (insert x no)

toList :: Arvore a -> [a]
toList Folha = []
toList (No l x r) = toList l ++ [x] ++ toList r

-- i)
elem :: (Ord a) => a -> Arvore a -> Bool
elem _ Folha = False
elem a (No l x r)
  | a == x = True
  | a < x = elem a l
  | otherwise = elem a r

-- j)
allIn :: (Ord a) => Arvore a -> Arvore a -> Bool
allIn Folha _ = True
allIn (No l x r) tree = verify x tree && allIn l tree && allIn r tree
  where
    verify _ Folha = False
    verify n (No l x r)
      | n == x = True
      | n < x = verify n l
      | otherwise = verify n r

-- Pesquisar se uma arvore esta em ordem

maiorElemento :: (Ord a) => Arvore a -> a
maiorElemento Folha = error "Error, Arvore nao pode estar vazia"
maiorElemento (No Folha x Folha) = x
maiorElemento (No left x Folha) = max x $ maiorElemento left
maiorElemento (No Folha x right) = max x $ maiorElemento right
maiorElemento (No left x right) = maximum [maiorElemento left, x, maiorElemento right]

-- Inverter arvore
invertArvore :: Arvore a -> Arvore a
invertArvore Folha = Folha
invertArvore (No left x right) = No (invertArvore right) x (invertArvore left)

-- Verifica quantas folhas tem a arvore
contaFolha :: Arvore a -> Int
contaFolha Folha = 0
contaFolha (No Folha _ Folha) = 1
contaFolha (No left _ right) = contaFolha left + contaFolha right

-- verificar se a arvore a direita e subArvore da esquerda
isSubTree :: (Eq a) => Arvore a -> Arvore a -> Bool
isSubTree _ Folha = True
isSubTree Folha _ = False
isSubTree current@(No l x r) sub@(No cl cx xr)
  | aux current sub = True
  | otherwise = isSubTree l sub || isSubTree r sub
  where
    aux Folha Folha = True
    aux (No l x r) (No cl cx xr) = x == cx && aux l cl && aux r xr
    aux _ _ = False

isEqual :: (Eq a) => Arvore a -> Arvore a -> Bool
isEqual Folha Folha = True
isEqual Folha _ = False
isEqual _ Folha = False
isEqual (No l x r) (No cl cx cr) = x == cx && isEqual l cl && isEqual r cr

instance (Eq a) => Eq (Arvore a) where
  Folha == Folha = True
  Folha == _ = False
  _ == Folha = False
  (No l x r) == (No cl cx cr) = x == cx && l == cl && r == cr

instance Functor Arvore where
  fmap _ Folha = Folha
  fmap f (No l x r) = No (fmap f l) (f x) (fmap f r)

instance Applicative Arvore where
  pure x = No Folha x Folha
  Folha <*> _ = Folha
  _ <*> Folha = Folha
  (No l f r) <*> (No cl cx cr) = No (l <*> cl) (f cx) (r <*> cr)

-- instance (Num a) => Num (Arvore a) where
--   (+) :: Num a => Arvore a -> Arvore a -> Arvore a
--   Folha + Folha = Folha
--   Folha + a = a
--   a + Folha = a
--   (No fl fx fr) + (No sl sx sr) = No (fl + sl) (fx + sx) (fr + sr)

instance (Arbitrary a) => Arbitrary (Arvore a) where
  arbitrary = sized genArvore
    where
      genArvore 0 = return Folha
      genArvore n =
        frequency
          [ (1, return Folha),
            ( n,
              do
                x <- arbitrary
                left <- genArvore (n `div` 2)
                right <- genArvore (n `div` 2)
                return (No left x right)
            )
          ]

(||\) :: (Eq a) => Arvore a -> Arvore a -> Bool
Folha ||\ Folha = True
Folha ||\ _ = False
_ ||\ Folha = False
(No l x r) ||\ (No cl cx cr) = x == cx && l ||\ cl && r ||\ cr

prop_isEqual :: (Eq a) => Arvore a -> Bool
prop_isEqual a = a == (invertArvore (invertArvore a))

teste :: Arvore a -> Bool
teste x = isPerfect x == isPerfect' x

prop_maiorElem :: (Num a, Ord a) => Arvore a -> Property
prop_maiorElem arvore = arvore /= Folha && maiorT arvore ==> maiorElemento arvore >= 2
  where
    maiorT (No _ x _) = x > 2
