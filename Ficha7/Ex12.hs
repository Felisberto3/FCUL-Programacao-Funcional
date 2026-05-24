import Ex11

instance Functor Set where
  fmap f (S list) = S (map f list)