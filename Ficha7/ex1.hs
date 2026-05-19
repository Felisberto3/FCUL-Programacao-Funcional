data Shape = Cicle Float | Retangle Float Float | Triangle Float Float Float

perimetro :: Shape -> Float
perimetro (Cicle r) = 2 * pi * r
perimetro (Retangle a b) = 2 * (a + b)
perimetro (Triangle x y z) = x + y + z

area :: Shape -> Float
area (Cicle r) = pi * r ^ 2
area (Triangle a b _) = (a * b) / 2
area (Retangle b h) = b * h