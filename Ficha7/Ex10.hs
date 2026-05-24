import Ex9

instance Functor Arvore where
  fmap f Folha = Folha
  fmap f (No l x r) = No (fmap f l) (f x) (fmap f r)
