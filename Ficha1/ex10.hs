-- 10. Escreva uma função que devolva quantos números entre 1 e n têm o seu
-- quadrado par.

quadradoPar n = [x | x <- [1 .. n], even (x ^ 2)]

-- Sabendo que quadrodo de um impar é sempre impar
quadradoPar2 n = [x | x <- [1 .. n], even x]