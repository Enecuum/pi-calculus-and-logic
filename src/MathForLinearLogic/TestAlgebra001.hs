{-# LANGUAGE GADTs, ExistentialQuantification, FlexibleInstances, DatatypeContexts, StandaloneDeriving, UndecidableInstances, TypeFamilies, AllowAmbiguousTypes, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, IncoherentInstances #-}

module MathForLinearLogic.TestAlgebra001 where

newtype FixF f = InF ( f (FixF f) )

instance Show (f (FixF f)) => Show (FixF f) where
  show (InF a) = show a

type Term = FixF (TermBF Integer)
type TermF a = TermBF Integer a

type family (OutF a) where
  OutF (FixF f) = f (FixF f)
  OutF a = a

class Fixable a where
  inF :: OutF a -> a
  outF :: a -> OutF a

instance {-# OVERLAPPING #-} (OutF (FixF f) ~ f (FixF f)) => Fixable (FixF f) where
  inF = InF
  outF (InF a) = a

-- instance {-# OVERLAPPABLE #-} (OutF q ~ q, q ~ Integer) => Fixable q where
instance {-# OVERLAPPABLE #-} (OutF q ~ q) => Fixable q where
  inF q = q
  outF q = q

data TermBF a b
  = U | W | N a
  | OutF b `Add` OutF b
  | OutF b `Sub` OutF b
  | OutF b `Mul` OutF b
  | OutF b `Div` OutF b
  | OutF b `Mod` a
  | OutF b `Pow` a

deriving instance Show (TermBF Integer Integer)
deriving instance Show (TermF Term)

infixr 7 `Add`
infixr 7 `Sub`
infixr 8 `Mul`
infixr 8 `Div`

n :: Integer -> Term
n x = InF $ N x

instance Num Term where
  a - b = reduce $ InF (outF a `Sub` outF b)
  a + b = reduce $ InF (outF a `Add` outF b)
  a * b = reduce $ InF (outF a `Mul` outF b)
  fromInteger n = InF $ N n

reduce :: Term -> Term
reduce a = a

class CondBifunctorM t where
  condBimapM :: (Monad m, Fixable a, Fixable b, Fixable c, Fixable d) => (t a b -> Bool) -> (a -> m c) -> (b -> m d) -> t a b -> m (t c d)

instance CondBifunctorM TermBF where
  condBimapM p f j o@(a `Add` b) | p o = do c <- j (inF a); d <- j (inF b); return (outF c `Add` outF d)
  condBimapM p f j o@(a `Sub` b) | p o = do c <- j (inF a); d <- j (inF b); return (outF c `Sub` outF d)
  condBimapM p f j o@(a `Mul` b) | p o = do c <- j (inF a); d <- j (inF b); return (outF c `Mul` outF d)
  condBimapM p f j o@(a `Div` b) | p o = do c <- j (inF a); d <- j (inF b); return (outF c `Div` outF d)
  condBimapM p f j o@(a `Mod` b) | p o = do c <- j (inF a); d <- f      b ; return (outF c `Mod`      d)
  condBimapM p f j o@(a `Pow` b) | p o = do c <- j (inF a); d <- f      b ; return (outF c `Pow`      d)
  condBimapM p f j o@(N a) | p o = do b <- f a; return (N b)

test01 :: TermBF Integer Integer
test01 = 1 `Add` 2

test02 = print "yes" >> condBimapM (const True) return (\a -> return $ a + 1) test01

test03 :: Term
test03 = ( 2 + 3 ) * 4

{-
test04 = print "yes" >> cataM (const True) f test03
 where
  f (InF (N a)) = return $ InF $ N (a+1)
  f a = return  a
-}


cataM :: (CondBifunctorM t, Monad m, Fixable a, Fixable b) => (t a (FixF (t a)) -> Bool) -> (t a b -> m b) -> FixF (t a) -> m b
cataM p f a = f =<< condBimapM p return (cataM p f) (outF a)

{-
termAnaM :: Monad m => (Term -> Bool) -> (Term -> m Term) -> Term -> m Term
termAnaM p f a = f a >>= termMapM p (termAnaM p f)

termHyloM :: Monad m => (Term -> Bool) -> (Term -> m Term) -> (Term -> m Term) -> Term -> m Term
termHyloM p f g a = g a >>= termMapM p (termHyloM p f g) >>= f

termParaM :: Monad m => (Term -> Bool) -> ((Term, Term) -> m Term) -> Term -> m Term
termParaM p f a = termMapM p (termParaM p f) a >>= curry f a

termApoM :: Monad m => (Term -> Bool) -> (Term -> m (Either Term Term)) -> Term -> m Term
termApoM p f a = do
  b <- f a
  case b of
    Left  c -> return c
    Right d -> termMapM p (termApoM p f) d

termCata p f   a = runIdentity $ termCataM p (return . f) a
termAna  p f   a = runIdentity $ termAnaM  p (return . f) a
termHylo p f g a = runIdentity $ termHyloM p (return . f) (return . g) a
termPara p f   a = runIdentity $ termParaM p (return . f) a
termApo  p f   a = runIdentity $ termApoM  p (return . f) a
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
