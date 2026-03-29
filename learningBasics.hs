import Data.Type.Coercion (trans)

factorial 0 = 1
factorial x = x * factorial (x - 1)

f 0 a b = a
f x a b = f (x - 1) b (a + b)

-- diferente way to do that
fact n = product [1 .. n]

listNf = [(x, f x 0 1) | x <- [0 ..]]

--  using function sum and product

sumElement n = sum ([y | (x, y) <- take n listNf])

productElement n = product ([y | (x, y) <- take n listNf, y /= 0])

-- enumerate upperCase and lowerCase latter
enumUpper w = length [x | x <- w, x `elem` ['A' .. 'Z']]

enumLower w = length [x | x <- w, elem x ['a' .. 'z']]

-- using cycle, it works like cycle [1,2,3] and the result is [1,2,3,1,2,3,1,2,3,1,2,3..]

cycleL list n = take n (cycle list)

-- using repeat, it works like:  repeat 5 the result is [5,5,5,5,5,5,5,5..]
oneElemL element lengthL = take lengthL (repeat element)

-- numero Binarios

b 0 = [0]
b 1 = [1]
b x = b (div x 2) ++ [mod x 2]

-- como usar o | no haskell

-- testing
list = [1 .. 4]

list2 = [-2 .. 4]

r = zip list list2

c :: Double -> Double
c r = 2 * pi * r

x :: Int
x = 2

div' :: (Fractional y) => y -> y -> y
div' y b = ((4 + y + b) / (fromIntegral x + b)) / b

-- max in list (great!)
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)

replicate' :: (Integral b) => b -> a -> [a]
replicate' 0 x = []
replicate' n x = x : replicate' (n - 1) x

-- curried functions
novaList = map (\x -> x ^ 2 + 1)

map' :: (a -> a) -> [a] -> [a]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x : xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

zipWith' :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys