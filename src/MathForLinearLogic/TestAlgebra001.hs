{-# LANGUAGE GADTs, ExistentialQuantification, FlexibleInstances, DatatypeContexts, StandaloneDeriving, UndecidableInstances #-}

module MathForLinearLogic.TestAlgebra001 where

newtype FixF f = InF { outF :: f (FixF f) }

instance Show (f (FixF f)) => Show (FixF f) where
  show (InF a) = show a

type Term = TermF TermA
type TermA = FixF (TermBF Integer)
type TermF a = TermBF Integer a

data (a ~ Integer, b ~ TermA) => TermBF a b
  = U | W | N Integer
  | Term `Add` Term
  | Term `Sub` Term
  | Term `Mul` Term
  | Term `Div` Term
  | Term `Mod` Integer
  | Term `Pow` Integer
deriving instance Show Term

infixr 7 `Add`
infixr 7 `Sub`
infixr 8 `Mul`
infixr 8 `Div`

n :: Integer -> Term
n x = N x

instance Num Term where
  a - b = reduce (a `Sub` b)
  a + b = reduce (a `Add` b)
  a * b = reduce (a `Mul` b)
  fromInteger n = N n

reduce :: Term -> Term
reduce a = a

{-
termCondBimapM :: Monad m => (Term -> Bool) -> (Integer -> m Integer) -> (Term -> m Term) -> Term -> m Term
termCondBimapM p f j o@(a `Add` b) | p o = do c <- j a; d <- j a; return (c `Add` d)
termCondBimapM p f j o@(N a) | p o = do b <- f a; return (N b)
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
