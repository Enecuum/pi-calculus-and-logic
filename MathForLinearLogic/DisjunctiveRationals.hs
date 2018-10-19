module MathForLinearLogic.DisjunctiveRationals where

data LLAlg = Add LLAlg LLAlg | Mul LLAlg LLAlg | Div LLAlg LLAlg | Share Int | Use Int | Nat Integer
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
  fromInteger n = Nat n


instance Num LLRat where
  LLRat a b + LLRat c d = llRatReduce $ LLRat (a*d + c*b) (b*d)
  LLRat a b * LLRat c d = llRatReduce $ LLRat (a*c) (b*d)

  fromInteger n = LLRat (Nat n) (Nat 1)

instance Fractional LLRat where
  LLRat a b / LLRat (Nat 1) (Nat 1) = LLRat b a

llAlgNormalize a = a

llRatReduce a = a















