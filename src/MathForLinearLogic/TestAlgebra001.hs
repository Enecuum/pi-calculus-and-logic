{-# LANGUAGE GADTs, ExistentialQuantification, FlexibleInstances, DatatypeContexts, StandaloneDeriving, UndecidableInstances, TypeFamilies, AllowAmbiguousTypes, FlexibleContexts #-}

module MathForLinearLogic.TestAlgebra001 where

newtype FixF f = InF ( f (FixF f) )

instance Show (f (FixF f)) => Show (FixF f) where
  show (InF a) = show a

-- type Term = FixF (TermBF Integer)
-- type TermF a = TermBF Integer a

{-
type family (OutF a) where
  OutF (FixF f) = f (FixF f)
-}

type family InF a where
  InF (f (FixF f)) = FixF f

class Fixable a where
  type OutF a
  inF :: OutF a -> a
  -- outF :: InF (OutF a) -> OutF a
  outF :: a -> OutF a

instance Fixable (FixF f) where
  type OutF (FixF f) = f (FixF f)
  inF = InF
  outF (InF a) = a

data TermBF a b where
  U :: TermBF a b
  W :: TermBF a b
  N :: a -> TermBF a b
  Add :: TermBF a (FixF (TermBF a)) -> TermBF a (FixF (TermBF a))  -> TermBF a (FixF (TermBF a))
  Sub :: TermBF a (FixF (TermBF a)) -> TermBF a (FixF (TermBF a))  -> TermBF a (FixF (TermBF a))
  Mul :: TermBF a (FixF (TermBF a)) -> TermBF a (FixF (TermBF a))  -> TermBF a (FixF (TermBF a))
  Div :: TermBF a (FixF (TermBF a)) -> TermBF a (FixF (TermBF a))  -> TermBF a (FixF (TermBF a))
  Mod :: TermBF a (FixF (TermBF a)) -> a -> TermBF a (FixF (TermBF a))
  -- Pow :: TermBF a (FixF (TermBF a)) -> a -> TermBF a (FixF (TermBF a))
  -- Pow :: (Fixable b, Fixable (InF (OutF b))) => OutF b -> a -> TermBF a b
  Pow :: (InF (OutF b) ~ b, Fixable (InF (OutF b))) => OutF b -> a -> TermBF a b

{-
data Fixable b => TermBF a b
  = U | W | N a
  | OutF b `Add` OutF b
  | OutF b `Sub` OutF b
  | OutF b `Mul` OutF b
  | OutF b `Div` OutF b
  | OutF b `Mod` a
  | OutF b `Pow` a
-}

-- deriving instance Show (TermF Term)

infixr 7 `Add`
infixr 7 `Sub`
infixr 8 `Mul`
infixr 8 `Div`

-- n :: Integer -> Term
-- n x = InF $ N x

{-
instance Num Term where
  a - b = reduce $ InF (outF a `Sub` outF b)
  a + b = reduce $ InF (outF a `Add` outF b)
  a * b = reduce $ InF (outF a `Mul` outF b)
  fromInteger n = InF $ N n

reduce :: Term -> Term
reduce a = a

termCondBimapM :: Monad m => (TermBF a b -> Bool) -> (a -> m a) -> (b -> m b) -> TermBF a b -> m (TermBF a b)
termCondBimapM = undefined
-}

{-
termCondBimapM :: Monad m => (Term -> Bool) -> (Integer -> m Integer) -> (Term -> m Term) -> Term -> m Term
termCondBimapM p f j o@(InF (a `Add` b)) | p o = do InF c <- j (InF a); InF d <- j (InF b); return (InF (c `Add` d))
termCondBimapM p f j o@(InF (a `Sub` b)) | p o = do InF c <- j (InF a); InF d <- j (InF b); return (InF (c `Sub` d))
termCondBimapM p f j o@(InF (a `Mul` b)) | p o = do InF c <- j (InF a); InF d <- j (InF b); return (InF (c `Mul` d))
termCondBimapM p f j o@(InF (a `Div` b)) | p o = do InF c <- j (InF a); InF d <- j (InF b); return (InF (c `Div` d))
termCondBimapM p f j o@(InF (a `Mod` b)) | p o = do InF c <- j (InF a);     d <- f      b ; return (InF (c `Mod` d))
termCondBimapM p f j o@(InF (a `Pow` b)) | p o = do InF c <- j (InF a);     d <- f      b ; return (InF (c `Pow` d))
termCondBimapM p f j o@(InF (N a)) | p o = do b <- f a; return (InF (N b))
-}

class CondBifunctorM t where
  -- type OutF (FixF (t a))
  condBimapM :: Monad m => (t a b -> Bool) -> (a -> m c) -> (b -> m d) -> t a b -> m (t c d)

--instance (OutF (FixF (TermBF a)) ~ TermBF a (FixF (TermBF a))) => CondBifunctorM TermBF where
--instance (TermBF a b, OutF b ~ FixF (TermBF a)) => CondBifunctorM (TermBF :: * -> * -> *) where
instance CondBifunctorM TermBF where
  -- type OutF (FixF (TermBF a)) = TermBF a (FixF (TermBF a))
  condBimapM p f j o@(a `Pow` b) | p o = do c <- jj j a; d <- f b; return (c `Pow` d)
   where
    jj :: (Fixable b, Fixable d, Monad m) => (b -> m d) -> OutF b -> m (OutF d)
    jj j a = do b <- j (inF a); return undefined
  condBimapM p f j o@(N a) | p o = do b <- f a; return (N b)


{-
jj :: Monad m => a -> m (OutF b)
jj a = undefined
-}

-- TermBF OutF b -> b


-- cataM :: (CondBifunctorM t, Monad m) => (t a (FixF (t a)) -> Bool) -> (FixF (t a) -> m b) -> FixF (t a) -> m b
-- cataM p f a = condBimapM p return ((condBimapM p return f . outF) >>= (return . InF)) (outF a) >>= f

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
