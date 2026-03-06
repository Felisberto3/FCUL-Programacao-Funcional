factorial 0 = 1
factorial x = x * factorial (x - 1)

f 0 a b = a
f x a b = f (x - 1) b (a + b)

listNf = [(x, f x 0 1) | x <- [0 ..]]

--  using function sum and product

sumElement n = sum ([y | (x, y) <- take n listNf])

productElement n = product ([y | (x, y) <- take n listNf, y /= 0])