-- verifica se o tamanho da list é maior que 10
-- @params list é uma list
-- @return True se a lista é maior que 10 e False caso contrario
-- a)
funcaoMaiorQue10 list = length list > 10

-- b)
isVazio list = null list

isVazio' :: [a] -> Bool
isVazio' = null

-- c)
takeOutElement :: String -> String
takeOutElement a = init (tail a)

takeOutElement' :: [a] -> [a]
takeOutElement' = init . tail

-- d)
secondElement :: [a] -> a
secondElement list = list !! 1

-- e)
beforeLast :: [a] -> a
beforeLast list = last (init list)

beforeLast2 list = list !! (length list - 2)

-- f)
listElement list n = list !! (n - 1)

-- rewrite
-- beforeLast2

beforeLast3 :: [c] -> c
beforeLast3 = last . init

-- secondElement
secondElement2 :: [c] -> c
secondElement2 = head . tail

-- g)
reverte list = head list : reverse (tail list)

-- h)
somatorio list = sum (take 5 list)

-- i)
somatorio2 list n = sum (take n list)

-- j)
func list1 list2 = last list1 == last list2 && length list1 == length list2 && not (null list1 && null list2)