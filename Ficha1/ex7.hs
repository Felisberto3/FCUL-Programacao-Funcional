-- 7. Escreva uma função que divide o intervalo entre dois valores em n
-- partições iguais. O resultado deverá ser uma lista de n+1 elementos
-- onde a primeira partição é dada pelos primeiros dois elementos da lista,
-- a segunda partição pelo segundo e terceiro elementos da lista, e assim
-- adiante. Por exemplo:
particao a b n = [a + ((b - a) / n) * x | x <- [0 .. n]]