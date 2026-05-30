class Visible a where
  toString :: a -> String
  dimension :: a -> Int

instance Visible Char where
  toString c = [c]
  dimension _ = 1

instance Visible Bool where
  toString = show
  dimension False = 0
  dimension True = 1

instance Visible Int where
  toString = show
  dimension _ = 1

instance (Visible a) => Visible [a] where
  toString list = "{ " ++ unwords (map toString list) ++ "}"
  dimension = length

instance (Visible a, Visible b) => Visible (a, b) where
  toString (a, b) = "[" ++ toString a ++ "," ++ toString b ++ "]"
  dimension (_, _) = 2