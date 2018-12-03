{-# LANGUAGE GADTs, ExistentialQuantification, FlexibleInstances, StandaloneDeriving, UndecidableInstances, TypeFamilies, AllowAmbiguousTypes, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, IncoherentInstances, TupleSections, TypeOperators, DataKinds, CPP #-}

module Control.Morphisms.Prelude where

import Data.Functor.Identity
import Control.Monad
import GHC.TypeLits
import Data.Proxy
import Data.Dynamic


#define TOCDD(t,a) \
    ToCDD (GetRecordNameByIndex 0 (FirstPrototype t) "NotFound") a \
  , ToCDD (GetRecordNameByIndex 1 (FirstPrototype t) "NotFound") a \
  , ToCDD (GetRecordNameByIndex 2 (FirstPrototype t) "NotFound") a \
  , ToCDD (GetRecordNameByIndex 3 (FirstPrototype t) "NotFound") a \
  , ToCDD (GetRecordNameByIndex 4 (FirstPrototype t) "NotFound") a \
  , ToCDD (GetRecordNameByIndex 5 (FirstPrototype t) "NotFound") a \
  , ToCDD (GetRecordNameByIndex 6 (FirstPrototype t) "NotFound") a \
  , ToCDD (GetRecordNameByIndex 7 (FirstPrototype t) "NotFound") a \


class CondBifunctorM t where
  type FirstPrototype t
  condBimapM :: ( Monad m, Fixable a, Fixable b, Fixable c, Fixable d, TOCDD(t,a) )
             => (t a b -> Bool) -> (a -> m c) -> (b -> m d) -> t a b -> m (t c d)

class ToCDD (a :: Symbol) c where
  toCDD :: Proxy a -> TypeFromRecord a c -> c
  toCDD = undefined

instance ToCDD a (Record a b) where
  toCDD _ b = Record b

instance (TypeFromRecord a c ~ TypeNotFound) => ToCDD a (Record a b :@ c) where
  toCDD _ b = CommDisj $ Left  $ Record b

instance (ToCDD a c, TypeFromRecord a (d :@ e) ~ TypeNotFound) => ToCDD a (c :@ (d :@ e)) where
  toCDD a b = CommDisj $ Left  $ toCDD a b

instance (ToCDD a d, TypeFromRecord a c ~ TypeNotFound) => ToCDD a (c :@ d) where
  toCDD a b = CommDisj $ Right $ toCDD a b

fromCDD = undefined


type family CountRecords a where
  CountRecords (Record a b) = 1
  CountRecords (a :@ b) = CountRecords a + CountRecords b

type family GetRecordNameByIndex (a :: Nat) b c where
  GetRecordNameByIndex 0 (Record a b) c = a
  GetRecordNameByIndex n (a :@ b) c = GetRecordNameByIndex n a (GetRecordNameByIndex (n - CountRecords a) b "NotFound")
  GetRecordNameByIndex n a b = b

condBimap p f j   a = runIdentity $ condBimapM p (return . f) (return . j)              a
condCata  p f     a = runIdentity $ condCataM  p (return . f)                           a
condAna   p f     a = runIdentity $ condAnaM   p (return . f)                           a
condHylo  p f e g a = runIdentity $ condHyloM  p (return . f) (return . e) (return . g) a
condPara  p f     a = runIdentity $ condParaM  p (return . f)                           a
condApo   p f     a = runIdentity $ condApoM   p (return . f)                           a



newtype FixF f = InF ( f (FixF f) )

instance Show (f (FixF f)) => Show (FixF f) where
  show (InF a) = show a

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


data AnyType

type family Match a b where
  Match AnyType a = 'True
  


data TypeNotFound
newtype a :@ b = CommDisj (Either a b)
newtype Record (a :: Symbol) b = Record b

fromCDL :: a :@ b -> a
fromCDL = undefined

fromCDR :: a :@ b -> b
fromCDR = undefined

type family TypeXOR a b where
  TypeXOR TypeNotFound a = a
  TypeXOR a TypeNotFound = a
  TypeXOR a b = TypeNotFound

type family TypeFromRecord (a :: Symbol) b where
  TypeFromRecord a (Record a b) = b
  TypeFromRecord a (b :@ c) = TypeXOR (TypeFromRecord a b) (TypeFromRecord a c)
  TypeFromRecord a  b       = TypeNotFound



condParaM :: (CondBifunctorM t,                            Monad m,                            Fixable a, TOCDD(t,a))
          => (t a (FixF (t a)) -> Bool)                -> (t a (FixF (t a), b) -> m b)                                 -> FixF (t a)   -> m b
condParaM     p f     a =                           f =<<  condBimapM p return
                                                          (\fx -> do b <- condParaM p f fx; return (a,b))                (outF a)

condCataM :: (CondBifunctorM t,                            Monad m,                 Fixable a, Fixable b, TOCDD(t,a))
          => (t a (FixF (t a)) -> Bool)                -> (t a b -> m b)                                               -> FixF (t a)   -> m b
condCataM     p f     a =                           f =<<  condBimapM p return (condCataM p f)                           (outF a)


condHyloM :: (CondBifunctorM t
             ,CondBifunctorM f,                            Monad m,      Fixable b, Fixable c, Fixable d, TOCDD(t,a),TOCDD(f,c))
          => (f c d -> Bool)                           -> (t a b -> m      b ) ->  (f c b  ->  m (t a b))
                                                       -> (d     -> m (f c d))                                         -> d            -> m b
condHyloM     p f e g a =                     f =<< e =<<  condBimapM p return (condHyloM p f e g)                    =<< g a



condAnaM  :: (CondBifunctorM t,                            Monad m,                 Fixable a, Fixable b, TOCDD(t,a))
          => (t a b -> Bool)                           -> (b     -> m (t a b))                                         -> b            -> m (FixF (t a))
condAnaM      p f     a =              (return . InF) =<<  condBimapM p return (condAnaM p f)                         =<< f a


condApoM  :: (CondBifunctorM t,                            Monad m,                            Fixable a, TOCDD(t,a))
          => (t a (Either (FixF (t a)) b) -> Bool)     -> (b     -> m ( t a (Either (FixF (t a)) b) )   )              -> b            -> m (FixF (t a))
condApoM      p f     a = do
  b <- f a
  c <- condBimapM p return j b
  return $ inF c
 where
  j (Left  d) = return d
  j (Right d) = condApoM p f d




