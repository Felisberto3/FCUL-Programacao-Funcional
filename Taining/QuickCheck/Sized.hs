import Test.QuickCheck

genListSized :: Gen [Int]
genListSized = sized (\n -> vectorOf n arbitrary)