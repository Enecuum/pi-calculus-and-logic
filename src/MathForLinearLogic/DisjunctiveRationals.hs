module MathForLinearLogic.DisjunctiveRationals where

data LLAlg = Add LLAlg LLAlg | Mul LLAlg LLAlg | Div LLAlg LLAlg | Inv LLAlg | Neg LLAlg
           | Share Int | Use Int | Nat Integer | LimToZero | BottomN LLAlg | BottomD LLAlg
 deriving (Show)

data LLRat = LLRat LLAlg LLAlg
 deriving (Show)

instance Eq LLAlg where

instance Ord LLAlg where

instance Real LLAlg where

instance Enum LLAlg where

instance Integral LLAlg where
  toInteger (Nat a) = a

instance Num LLAlg where
  a + b = llAlgNormalize $ Add a b
  a * b = llAlgNormalize $ Mul a b
  fromInteger 0 = LimToZero
  fromInteger n = Nat n

instance Fractional LLAlg where
  a / b = llAlgNormalize $ Div a b


instance Num LLRat where
  LLRat a b + LLRat c d = llRatReduce $ LLRat (a*d + c*b) (b*d)
  LLRat a b * LLRat c d = llRatReduce $ LLRat (a*c) (b*d)

  fromInteger n = LLRat (Nat n) (Nat 1)

instance Fractional LLRat where
  LLRat a b / LLRat (Nat 1) (Nat 1) = LLRat b a
  a / b = a * (b / 1)

-- Multiplicative inverse for linear logic, with ⊥ (Bottom)
inv (LLRat a b) = llRatReduce $ LLRat (BottomN b) (BottomD a)

a & b = inv ( inv a + inv b )

a % b = inv ( inv a * inv b )

llAlgNormalize a = llAlgMultiplicativeNormalForm $ llAlgAdditiveNormalForm a

llRatReduce a = a



llAlgMultiplicativeNormalForm a = a

llAlgAdditiveNormalForm a = a












