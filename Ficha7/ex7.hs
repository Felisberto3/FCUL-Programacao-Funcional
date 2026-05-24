import Ex5

instance (Eq a) => Eq (Arvore a) where
  Folha == Folha = True
  (No l x r) == (No cl cx cr) = x == cx && l == cl && r == cr
  _ == _ = False
