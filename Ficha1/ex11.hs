-- 11. Dizemos que triplo (x, y, z) é Pitagórico se x^2 +y^2 = z^2. Utilizando uma
-- lista em compreensão defina a função pitagoricos que calcule a lista
-- de todos os triplos pitagóricos até um dado limite. Evite colocar triplos
-- que representem o mesmo triângulo, por exemplo (3,4,5) e (4,3,5).

-- ghci> pitagoricos 10
-- [(3,4,5),(6,8,10)]

pitagoricos n = [(x, y, z) | x <- [1 .. n], y <- [x + 1 .. n], z <- [y + 1 .. n], x ^ 2 + y ^ 2 == z ^ 2]