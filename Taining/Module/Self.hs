module Self (introduce, Tree (..)) where

data Tree a = Folha | No (Tree a) a (Tree a) deriving (Show)

introduce :: String -> String
introduce n = "Meu nome e " ++ n