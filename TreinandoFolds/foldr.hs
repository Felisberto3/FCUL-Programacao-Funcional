-- soma de todos elementos
soma :: [Int] -> Int
soma = foldr (+) 0

-- produtos
produto :: [Int] -> Int
produto = foldr (*) 1

-- calc tamanho da lista
tamanho :: [a] -> Int
tamanho = foldr (\_ acc -> acc + 1) 0

-- somar pares
somaPares :: [Int] -> Int
somaPares = foldr (\x acc -> if even x then acc + x else acc) 0

-- quantos positivos
contaPositivos :: [Int] -> Int
contaPositivos = foldr (\x acc -> if x > 0 then acc + 1 else acc) 0
