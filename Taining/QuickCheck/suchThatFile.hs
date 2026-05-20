import Test.QuickCheck

gerarPares :: Gen Int
gerarPares = suchThat arbitrary even

listPares :: Gen [Int]
listPares = listOf $ suchThat arbitrary even

listComNelementos :: Int -> Gen [Int]
listComNelementos n = vectorOf n $ suchThat arbitrary odd

prop_PareVazia :: Property
prop_PareVazia =
  forAll listPares $ \xs -> null xs
