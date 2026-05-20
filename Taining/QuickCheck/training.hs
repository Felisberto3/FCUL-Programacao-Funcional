import Test.QuickCheck
import Test.QuickCheck (Property)

-- stdArgs::Args
-- stdArgs = Args
-- {
--     maxSuccess = 100,
--     maxDiscard = 500,
--     maxSize = 100,
--     chatty = True,
--     replay = Nothing,
-- }

soma :: (Num a, Eq a) => a -> Bool
soma x = x + 1 == x + 2

prop_errada :: [Int] -> Bool
prop_errada xs = reverse xs == xs

prop_reverseLength :: [a] -> Bool
prop_reverseLength xs = length (reverse xs) == length xs

prop_somaPositiva :: Int -> Int -> Bool
prop_somaPositiva x y = x + y > x

prop_lastIsHeadReverse :: (Eq a) => [a] -> Property
prop_lastIsHeadReverse xs = not (null xs) ==> last xs == head (reverse xs)

gerarLista2ouMais' :: Gen [Int]
gerarLista2ouMais' = do
  n <- choose (2, 20)
  vectorOf n arbitrary

gerarLista2ouMais :: Gen [Int]
gerarLista2ouMais = do
  x <- arbitrary
  y <- arbitrary
  resto <- arbitrary
  return (x : y : resto)

verificargenListMaior2 :: Property
verificargenListMaior2 =
  forAll gerarLista2ouMais $ \x -> length x >= 2

prop_verificarSeMaiorq2 :: Property
prop_verificarSeMaiorq2 = property $ do
  list <- gerarLista2ouMais
  return $ length list >= 2

prop_reverseClassify :: [Int] -> Property
prop_reverseClassify xs =
  classify (null xs) "Lista Vazia" $
    classify (length xs < 10) "Lista menores q 10" $
      classify (length xs > 10) "Lista maiores q 10" $
        reverse (reverse xs) == xs

prop_reverseCollect :: [Int] -> Property
prop_reverseCollect xs =
  collect (length xs) $
    reverse (reverse xs) == xs

data Cor = Vermelho | Verde | Azul | Branco deriving (Show, Eq, Enum)

instance Arbitrary Cor where
  arbitrary = elements [Vermelho, Verde, Azul, Branco]

gerarCor :: Gen [Cor]
gerarCor = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a : c : b)

prop_corRever :: Property
prop_corRever =
  forAll gerarCor (\xs -> length xs >= 28)

-- duvidas
-- 1. como e onde usar o (and e or)
-- 2. Como usar 1) no quickCheck