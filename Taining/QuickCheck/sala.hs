import Test.QuickCheck
import Test.QuickCheck (vectorOf)

data Turma = T [Aluno] deriving (Show)

data Aluno = A String [Int] deriving (Show)

mediaTurma :: Turma -> Int
mediaTurma (T listaAluno) = sum listaMedia `div` length listaMedia
  where
    listaMedia = map mediaAluno listaAluno
    mediaAluno (A _ notas) = sum notas `div` length notas

instance Arbitrary Aluno where
  arbitrary = do
    notas <- sequence (replicate 6 (choose (0, 20)))
    tamanho <- choose (3, 12)
    nome <- vectorOf tamanho (elements (['a' .. 'z'] ++ ['A' .. 'Z']))
    return (A nome notas)

instance Arbitrary Turma where
  arbitrary = T <$> arbitrary

y :: (Fractional a) => a -> a -> a
y a b = a / b