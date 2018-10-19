module MathForLinearLogic.DisjunctiveRationals where

data LLAlg = Add LLAlg LLAlg | Mul LLAlg LLAlg | Share Int | Use Int | Nat Integer
 deriving (Show)

data LLRat = LLRat Integer LLAlg
 deriving (Show)

instance Eq LLAlg where

instance Ord LLAlg where

instance Real LLAlg where

instance Enum LLAlg where

instance Integral LLAlg where
  toInteger a = 1

instance Num LLAlg where


instance Num LLRat where
  LLRat a b + LLRat c d = llRatReduce $ LLRat (a * toInteger d + c * toInteger b) (b*d)
  LLRat a b * LLRat c d = llRatReduce $ LLRat (a*c) (b*d)

  fromInteger n = LLRat n (Nat 1)


llRatReduce a = a















