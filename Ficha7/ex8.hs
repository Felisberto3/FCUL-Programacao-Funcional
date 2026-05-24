data Arvore a = Folha | No (Arvore a) a (Arvore a)

instance (Show a) => Show (Arvore a) where
  show Folha = "  Folha\n  "
  show (No l x r) = show x ++ "\n  " ++ show l ++ show r
