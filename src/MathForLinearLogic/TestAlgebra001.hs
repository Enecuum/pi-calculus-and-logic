{-# LANGUAGE GADTs, ExistentialQuantification, FlexibleInstances, DatatypeContexts #-}

module MathForLinearLogic.TestAlgebra001 where

newtype FixF f = InF { outF :: f (FixF f) }

type Term = TermF TermA
type TermA = FixF (TermBF Integer)
type TermF a = TermBF Integer a

data (a ~ Integer, b ~ TermA) => TermBF a b
  = U | W | N Integer
  | TermA `Add` TermA
  | TermA `Sub` TermA
  | TermA `Mul` TermA
  | TermA `Div` TermA
  | TermA `Mod` Integer
  | TermA `Pow` Integer

{-
data TermBF a b where
  U   :: Term
  W   :: Term
  N   :: Integer          -> Term
  Add :: TermA -> TermA   -> Term
  Sub :: TermA -> TermA   -> Term
  Mul :: TermA -> TermA   -> Term
  Div :: TermA -> TermA   -> Term
  Mod :: TermA -> Integer -> Term
  Pow :: TermA -> Integer -> Term
-}
  

{-
data Term a where
  U ::            Term Value
  W ::            Term Value
  N :: Integer -> Term Value
  Add :: Term a -> Term b -> Term Expr
  Sub :: Term a -> Term b -> Term Expr
  Mul :: Term a -> Term b -> Term Expr
  Div :: Term a -> Term b -> Term Expr
  Mod :: Term a -> Integer -> Term Expr
  Pow :: Term a -> Integer -> Term Expr
-}

{-
type PT = PrettyTerm
data PrettyTerm = PTerm (Term Value) | PT `PAdd` PT | PT `PSub` PT | PT `PMul` PT | PT `PDiv` PT | PT `PMod` Integer | PT `PPow` Integer
 deriving Show

infixr 7 `PAdd`
infixr 7 `PSub`
infixr 8 `PMul`
infixr 8 `PDiv`

instance Show (Term Value) where
  show U = "u"
  show W = "w"
  show (N n) = show n

termToPretty :: Term a -> PrettyTerm
termToPretty (Add a b) = termToPretty a `PAdd` termToPretty b
termToPretty (Sub a b) = termToPretty a `PSub` termToPretty b
termToPretty (Mul a b) = termToPretty a `PMul` termToPretty b
termToPretty (Div a b) = termToPretty a `PDiv` termToPretty b
termToPretty (Mod a b) = termToPretty a `PMod` b
termToPretty (Pow a b) = termToPretty a `PPow` b
termToPretty U = PTerm U
termToPretty W = PTerm W
termToPretty (N n) = PTerm (N n)

instance Show (Term Expr) where
  show a = show $ termToPretty a

n x = N x

instance Num (Term a) where
  a - b = reduce (a `Sub` b)
  a + b = reduce (a `Add` b)
  a * b = reduce (a `Mul` b)
  fromInteger n = N n

reduce a = a
-}

{-
data Term = U | W | N Integer
   | Term `Mul` Term
   | Term `Add` Term
   | Term `Div` Term
   | Term `Sub` Term
   | Term `Pow` Integer
   | Term `Mod` Integer
 deriving (Show)

termCondBimapM :: Monad m -> (Term -> Bool) -> (Term -> m Term) -> (Term -> m Term) -> Term -> m Term
termCondBimapM p f j o | p o = j o
-}



{-

instance Eq LLN where
  (a `Mul` b) == (c `Mul` d) = ( (a == c) && (b == d) ) || ( (a == d) && (b == c) )
  (a `Add` b) == (c `Add` d) = ( (a == c) && (b == d) ) || ( (a == d) && (b == c) )
  N a == N b = a == b
  a == b = show (reduce a) == show (reduce b)

infixr 7 `Add`
infixr 8 `Mul`
infixr 9 `Pow`

lli (a `Div` b) = reduce ( lli01 b `Div` lli01 a )
 where
  lli01 x = x * U + W
lli a = lli (a `Div` N 1)

reduce (a `Mul` b) | a > b = b `Mul` a
reduce (N 1 `Mul` U `Add` W) = N 1
reduce (U `Pow` N 2) = N 1
reduce (U `Mul` W `Add` W) = 0
reduce (a `Mul` b) | a == b = a `Pow` N 2
reduce (a `Mul` b `Pow` c) | a == b = a `Pow` (b `Add` N 1)
reduce (N a `Add` N b) = N (a+b)
reduce (N a `Mul` N b) = N (a*b)
reduce (N a `Sub` N b) = N (a-b)
reduce a = a

-}

{-

lli01(x) = (x*u+w)
lli01(lli01(x)) = x

(x*u+w)*u+w = x
(x*u+w)*u+w = x*u*u + u*w + w
(x*u+w)*u+w = x*1   + u*w + w
(x*u+w)*u+w = x*1   + 0

1*u+w = 1
u*u = 1
u*w + w = 0

lli(a/b) = lli01(b) / lli01(a)



-}
