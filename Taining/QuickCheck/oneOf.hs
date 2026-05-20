import Test.QuickCheck
import Test.QuickCheck (listOf)

data Exp = Num Int | Soma Exp Exp | Produto Exp Exp deriving (Show)

genArbitrary :: Gen Exp
genArbitrary =
  oneof
    [ Num <$> arbitrary,
      Soma <$> (Num <$> arbitrary) <*> (Num <$> arbitrary),
      Produto <$> (Num <$> arbitrary) <*> (Num <$> arbitrary)
    ]

genList :: Int -> Gen [Exp]
genList n = vectorOf n genArbitrary