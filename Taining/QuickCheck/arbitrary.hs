import Test.QuickCheck

data Ponto a = Ponto a a deriving (Eq, Show)

instance (Num a, Arbitrary a) => Arbitrary (Ponto a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Ponto x y)

data Ponto2 a = Ponto2 a a deriving (Eq, Show)

instance (Num a, Arbitrary a) => Arbitrary (Ponto2 a) where
  arbitrary = Ponto2 <$> arbitrary <*> arbitrary

data Ponto3 a = Ponto3 a a deriving (Eq, Show)

instance (Num a, Arbitrary a) => Arbitrary (Ponto3 a) where
  arbitrary = do
    x <- arbitrary
    Ponto3 x <$> arbitrary

data Ponto4 a = Ponto4 a a deriving (Eq, Show)

gerarPonto :: (Num a, Arbitrary a) => Gen [Ponto2 a]
gerarPonto = listOf arbitrary

data Ponto5 = Ponto5 Int Int deriving (Show)

instance Arbitrary Ponto5 where
  arbitrary = Ponto5 <$> (suchThat arbitrary (\x -> x > 0)) <*> suchThat arbitrary (\x -> x > 0)