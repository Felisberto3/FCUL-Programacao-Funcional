module ElementsFile (Naipe) where

import Test.QuickCheck

data Naipe = Copas | Ouro | Paus | Espadas deriving (Show)

genNaipe :: Gen Naipe
genNaipe = elements [Copas, Ouro, Paus, Espadas]

instance Arbitrary Naipe where
  arbitrary = elements [Copas, Ouro, Paus, Espadas]
