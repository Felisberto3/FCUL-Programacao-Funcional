import Ex13

instance (Show k, Show v) => Show (Map k v) where
  show (M list) = "{" ++ strings list ++ "}"
    where
      strings [] = ""
      strings [(k, v)] = show k ++ ":" ++ show v
      strings ((k, v) : xs) = show k ++ ":" ++ show v ++ "," ++ strings xs