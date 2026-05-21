import Ex5

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ z [] = z
foldr' f z (x : xs) = f x (foldr' f z xs)

foldl' :: (a -> b -> b) -> b -> [a] -> b
foldl' _ z [] = z
foldl' f z (x : xs) = foldl' f (f x z) xs

t = all even [1, 3, 5]

-- instance Foldable Arvore where
--   foldr f z Vazia      = z
--   foldr f z (No v esq dir) =foldr f (f v (foldr f z dir))  esq

-- foldr f z (No v e d) = foldr f (f v (foldr f z d)) e
---TESTANDO NOVOS APRENDIZADOS

frArvore :: (a -> b -> b) -> b -> Arvore a -> b
frArvore _ z Folha = z
frArvore f z (No l x r) = frArvore f (f x (frArvore f z r)) l

flArvore :: (a -> b -> b) -> b -> Arvore a -> b
flArvore _ z Folha = z
flArvore f z (No l x r) = flArvore f (f x (flArvore f z l)) r

fold :: (b -> a -> b -> b) -> b -> Arvore a -> b
fold _ e Folha = e
fold f e (No l x r) = f (fold f e l) x (fold f e r)

-- c) O número de nós na Árvore
size :: Arvore a -> Int
size = fold (\l x r -> 1 + l + r) 0

-- d)
depth :: Arvore a -> Int
depth = fold (\l x r -> 1 + max l r) 0

-- Falta alineas e, h, i, j
-- // Tipos malucos com foldr e foldl
-- // Instancia malucas de ord Arvore
-- // Instancia malucas de == Arvore
-- frArvore f z (No)
