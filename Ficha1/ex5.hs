-- verifica se o tamanho da list é maior que 10
-- @params list é uma list
-- @return True se a lista é maior que 10 e False caso contrario
funcaoMaiorQue10 list = length list > 10

isVazio list = null list

takeOutElement a = init (tail a)

secondElement list = list !! 1

beforeLast list = last (init list)

beforeLast2 list = list !! (length list - 2)

listElement list n = list !! (n - 1)

-- rewrite
-- beforeLast2

beforeLast3 :: [c] -> c
beforeLast3 = last . init

-- secondElement
secondElement2 :: [c] -> c
secondElement2 = head . tail