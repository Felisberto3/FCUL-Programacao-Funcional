import Test.QuickCheck
import Prelude hiding (pred)

data Nat = Zero | Succ Nat deriving (Show, Eq)

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)

menus :: Nat -> Nat -> Nat
menus Zero _ = Zero
menus (Succ n) Zero = Succ n
menus (Succ n) (Succ m) = menus n m

-- c)
pred :: Nat -> Nat
pred (Succ n) = n

-- d)
sub :: Nat -> Nat -> Nat
sub m Zero = m
sub Zero _ = error "Erro o segundo numero nao pode ser maior q o primeiro"
sub n m = sub (pred n) (pred m)

-- e)
mult :: Nat -> Nat -> Nat
mult x m = go x Zero
  where
    go Zero acc = acc
    go (Succ n) acc = go n (add acc m)

-- //outra forma de fazer:
mult' :: Nat -> Nat -> Nat
mult' Zero _ = Zero
mult' _ Zero = Zero
mult' (Succ n) m = add m (mult' n m)

-- f)
pot :: Nat -> Nat -> Nat
pot Zero _ = Zero
pot b Zero = Succ Zero
pot b (Succ n) = mult' b (pot b n)

-- g)
fat :: Nat -> Nat
fat Zero = Succ Zero
fat (Succ n) = mult' (Succ n) (fat n)

-- h)
lessThan :: Nat -> Nat -> Bool
lessThan _ Zero = False
lessThan Zero _ = True
lessThan (Succ n) (Succ m) = lessThan n m

-- teste
fromInt :: Int -> Nat
fromInt 0 = Zero
fromInt n = Succ (fromInt (n - 1))

toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ n) = 1 + toInt n

instance Arbitrary Nat where
  arbitrary = do
    n <- choose (0, 10)
    return (fromInt n)

prop_add :: Nat -> Nat -> Bool
prop_add a b = toInt (add a b) == toInt a + toInt b

prop_add2 :: Nat -> Nat -> Bool
prop_add2 a b = toInt (add a b) == toInt (add b a)

prop_menos :: Nat -> Bool
prop_menos a = toInt (menus a a) == 0

prop_menos2 :: Nat -> Nat -> Bool
prop_menos2 a b = toInt (menus (add a b) b) == toInt a
