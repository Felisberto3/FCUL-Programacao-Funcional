module Ex11 (Set (..)) where

data Set a = S [a]

instance (Show a) => Show (Set a) where
  show (S list) = "{" ++ listInString list ++ "}"
    where
      listInString [] = " "
      listInString [x] = show x
      listInString (x : xs) = show x ++ "," ++ listInString xs

instance (Eq a) => Eq (Set a) where
  S l1 == S l2 = all (`elem` l1) l2 && all (`elem` l2) l1