pares :: (Integral b) => b -> [(b, b)]
pares n = [(i, j) | j <- [1 .. n], i <- [1 .. n], even i, even j, i /= j]