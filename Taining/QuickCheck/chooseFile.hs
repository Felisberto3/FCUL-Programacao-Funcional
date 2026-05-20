import Test.QuickCheck

-- // gerar indices validos
gerarIndices :: Int -> Int -> Gen Int
gerarIndices i f = choose (i, f - 1)