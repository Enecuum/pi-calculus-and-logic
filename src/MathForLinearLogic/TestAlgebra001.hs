{-# LANGUAGE GADTs, ExistentialQuantification, FlexibleInstances, StandaloneDeriving, UndecidableInstances, TypeFamilies, AllowAmbiguousTypes, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, IncoherentInstances, TupleSections #-}

module MathForLinearLogic.TestAlgebra001 where

import Data.Functor.Identity
import Control.Monad

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

deriving instance (Show a, Show (OutF b)) => Show (TermBF a b)
deriving instance Show a => Show (TermBF a (FixF (TermBF a)))
deriving instance Show (TermF Term)

infixr 7 `Add`
infixr 7 `Sub`
infixr 8 `Mul`
infixr 8 `Div`

instance Num Term where
  a - b = reduce $ InF (outF a `Sub` outF b)
  a + b = reduce $ InF (outF a `Add` outF b)
  a * b = reduce $ InF (outF a `Mul` outF b)
  fromInteger n = InF $ N n

reduce :: Term -> Term
reduce a = a

{-
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
instance Eq LLN where
  (a `Mul` b) == (c `Mul` d) = ( (a == c) && (b == d) ) || ( (a == d) && (b == c) )
  (a `Add` b) == (c `Add` d) = ( (a == c) && (b == d) ) || ( (a == d) && (b == c) )
  N a == N b = a == b
  a == b = show (reduce a) == show (reduce b)
lli (a `Div` b) = reduce ( lli01 b `Div` lli01 a )
 where
  lli01 x = x * U + W
lli a = lli (a `Div` N 1)
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

class CondBifunctorM t where
  condBimapM :: (Monad m, Fixable a, Fixable b, Fixable c, Fixable d) => (t a b -> Bool) -> (a -> m c) -> (b -> m d) -> t a b -> m (t c d)

condBimap p f j a = runIdentity $ condBimapM p (return . f) (return . j) a

instance CondBifunctorM TermBF where
  condBimapM p f j o@(a `Add` b) | p o = do c <- j (inF a); d <- j (inF b); return (outF c `Add` outF d)
  condBimapM p f j o@(a `Sub` b) | p o = do c <- j (inF a); d <- j (inF b); return (outF c `Sub` outF d)
  condBimapM p f j o@(a `Mul` b) | p o = do c <- j (inF a); d <- j (inF b); return (outF c `Mul` outF d)
  condBimapM p f j o@(a `Div` b) | p o = do c <- j (inF a); d <- j (inF b); return (outF c `Div` outF d)
  condBimapM p f j o@(a `Mod` b) | p o = do c <- j (inF a); d <- f      b ; return (outF c `Mod`      d)
  condBimapM p f j o@(a `Pow` b) | p o = do c <- j (inF a); d <- f      b ; return (outF c `Pow`      d)
  condBimapM p f j o@(N a) | p o = do b <- f a; return (N b)
  condBimapM p f j U = return U


condParaM :: (CondBifunctorM t,                            Monad m,                            Fixable a)
          => (t a (FixF (t a)) -> Bool)                -> (t a (FixF (t a), b) -> m b)                                 -> FixF (t a)   -> m b
condParaM     p f     a =                           f =<<  condBimapM p return
                                                          (\fx -> do b <- condParaM p f fx; return (a,b))                (outF a)


condCataM :: (CondBifunctorM t,                            Monad m,                 Fixable a, Fixable b)
          => (t a (FixF (t a)) -> Bool)                -> (t a b -> m b)                                               -> FixF (t a)   -> m b
condCataM     p f     a =                           f =<<  condBimapM p return (condCataM p f)                           (outF a)


condHyloM :: (CondBifunctorM t
             ,CondBifunctorM f,                            Monad m,      Fixable b, Fixable c, Fixable d)
          => (f c d -> Bool)                           -> (t a b -> m      b ) ->  (f c b  ->  m (t a b))
                                                       -> (d     -> m (f c d))                                         -> d            -> m b
condHyloM     p f e g a =                     f =<< e =<<  condBimapM p return (condHyloM p f e g)                    =<< g a



condAnaM  :: (CondBifunctorM t,                            Monad m,                 Fixable a, Fixable b)
          => (t a b -> Bool)                           -> (b     -> m (t a b))                                         -> b            -> m (FixF (t a))
condAnaM      p f     a =              (return . InF) =<<  condBimapM p return (condAnaM p f)                         =<< f a


condApoM  :: (CondBifunctorM t,                            Monad m,                            Fixable a)
          => (t a (Either (FixF (t a)) b) -> Bool)     -> (b     -> m ( t a (Either (FixF (t a)) b) )   )              -> b            -> m (FixF (t a))
condApoM      p f     a = do
  b <- f a
  c <- condBimapM p return j b
  return $ inF c
 where
  j (Left  d) = return d
  j (Right d) = condApoM p f d



condCata p f     a = runIdentity $ condCataM p   (return . f)                           a
condAna  p f     a = runIdentity $ condAnaM  p   (return . f)                           a
condHylo p f e g a = runIdentity $ condHyloM p   (return . f) (return . e) (return . g) a
condPara p f     a = runIdentity $ condParaM p   (return . f)                           a
condApo  p f     a = runIdentity $ condApoM  p   (return . f)                           a














n :: Integer -> Term
n x = InF $ N x

test01 :: TermBF Integer Integer
test01 = 1 `Add` 2

test02 = condBimap (const True) id (+1) test01

test03 :: Term
test03 = ( 2 + 3 ) * 4

test04 = condCata (const True) f test03
 where
  f :: TermBF Integer Integer -> Integer
  f (a `Add` b) = (a+b)
  f (a `Mul` b) = (a*b)
  f (N n) = n
  f a = error (show a)

test06 :: FixF (TermBF Integer)
test06 = condAna (const True) f 1
 where
  f :: Integer -> (TermBF Integer Integer)
  f 5 = (N 99)
  f n = ( (n+1) `Add` (n+1) )

test07 :: FixF (TermBF Integer) -> Integer
test07 (InF U) = 0
test07 (InF (a `Mod` 1)) = test07 (InF a) + 1

test08 :: FixF (TermBF Integer)
test08 = InF $ foldr (\a b -> b `Mod` 1) U [1..5]

test09 :: Integer
test09 = condPara (const True) f test08
 where
  f U = 1
  f ((a,b) `Mod` 1) = test07 a * b
  f a = error (show a)














