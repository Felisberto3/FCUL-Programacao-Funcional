module Ex13 (Map (..)) where

data Map k a = M [(k, a)]

instance (Eq k, Eq a) => Eq (Map k a) where
  (M l1) == (M l2) = length l1 == length l2 && todosExistem l1 l2 && todosExistem l2 l1
    where
      isIn _ [] = False
      isIn x (y : ys) = x == y || isIn x ys
      todosExistem _ [] = True
      todosExistem (x : xs) l = isIn x l && todosExistem xs l