-- a)
func1 :: (Num a) => a -> a -> a -> a
func1 x y z = x + y + z

-- alinea b)
func :: (Ord a, Num a) => a -> a -> a -> a
func x y z =
  if x > 0 && y > 0 && z > 0
    then x + y + z
    else 0
