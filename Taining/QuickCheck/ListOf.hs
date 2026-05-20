import Test.QuickCheck

gerarList :: Gen [Int]
gerarList = listOf $ elements [1, 3, 50]