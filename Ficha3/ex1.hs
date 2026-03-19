-- 1.a)O primeiro elemento de um par
first :: (a, b) -> a
first (x, _) = x

-- (b) Um dado par com a ordem dos elementos trocados
par :: (a, b) -> (b, a)
par (x, y) = (y, x)

-- (c) O primeiro elemento de um triplo

triplo :: (x, y, z) -> x
triplo (x, _, _) = x

-- (d) Um dado triplo com os dois primeiros elementos trocados
triplo2 :: (x, y, z) -> (y, x, z)
triplo2 (x, y, z) = (y, x, z)

-- (e) O segundo elemento de uma lista
secondElement :: [a] -> a
secondElement [] = error "lista vazia"
secondElement [_] = error "lista com apenas um elemento"
secondElement (_ : x : _) = x

-- outra forma de resolver
sndElement2 list
  | null list || (length list == 1) = error "Elementos inferior a 1 nao pode!"
  | otherwise = list !! 1

sndElement3 :: [a] -> a
sndElement3 (_ : x : _) = x
sndElement3 _ = error "Elementos devem ter mais de um elemento"

-- (f) O segundo elemento do primeiro par de uma lista de pares
fstList :: [(a, b)] -> b
fstList ((_, x) : _) = x