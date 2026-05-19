data Nat = Zero | Succ Nat deriving (Show, Eq)

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)

menus :: Nat -> Nat -> Nat
menus Zero _ = Zero
menus (Succ n) Zero = Succ n
menus (Succ n) (Succ m) = menus n m