{-# LANGUAGE GADTs, ExistentialQuantification, FlexibleInstances, StandaloneDeriving, UndecidableInstances, TypeFamilies, AllowAmbiguousTypes, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, IncoherentInstances, TupleSections, TypeOperators, DataKinds #-}

module Control.Morphisms.Prelude where

import Data.Functor.Identity
import Control.Monad
import GHC.TypeLits
import Data.Proxy
import Data.Dynamic




class CondBifunctorM t where
  type FirstPrototype t
  condBimapM :: ( Monad m, Fixable a, Fixable b, Fixable c, Fixable d
                -- , Match a (FirstPrototype t) ~ 'True,
                -- , ToCDD "AllSymbols" (FirstPrototype t) a
                -- PathDataToFunction2 (TypeFromRecord "Name" a -> a)
                -- , ToCommDisjDict (FirstPrototype t)
                -- , ToCommDisjDict a
                , ToCommDisjDict a (ToCDD (FillPrototype (FirstPrototype t) a))
                )
             => (t a b -> Bool) -> (a -> m c) -> (b -> m d) -> t a b -> m (t c d)

{-
condBimap p f j   a = runIdentity $ condBimapM p (return . f) (return . j)              a
condCata  p f     a = runIdentity $ condCataM  p (return . f)                           a
condAna   p f     a = runIdentity $ condAnaM   p (return . f)                           a
condHylo  p f e g a = runIdentity $ condHyloM  p (return . f) (return . e) (return . g) a
condPara  p f     a = runIdentity $ condParaM  p (return . f)                           a
condApo   p f     a = runIdentity $ condApoM   p (return . f)                           a
-}

class ToCommDisjDict a b where
  toCDD :: a -> b

instance (TypeFromRecord a (Record a b) ~ b) =>  ToCommDisjDict (Record a b) (b -> Record a b) where
  toCDD _ = Record

instance (ToCommDisjDict a (c -> a), ToCommDisjDict b (d -> b)) => ToCommDisjDict (a :@ b) ( c -> (a :@ b) , d -> (a :@ b) ) where
  toCDD a = ( CommDisj . Left . toCDD (fromCDL a) , CommDisj . Right . toCDD (fromCDR a) )

instance ToCommDisjDict a () where
  toCDD _ = ()

type family ToCDD a where
  ToCDD (Record a b) = b -> Record a b
  ToCDD (a :@ b) = ( ToCDD a , ToCDD b )
  ToCDD a = ()

type family FillPrototype a b where
  FillPrototype (Record a b) (Record a c) = Record a c
  FillPrototype (a :@ b) (c :@ d) = FillPrototype a c :@ FillPrototype b d

{-
class ToCommDisjDict a where
  toCDD :: a -> [Dynamic]

instance Typeable (b -> Record a b) => ToCommDisjDict (Record a b) where
  toCDD a = [toDyn $ f a]
    where
     f :: Record a b -> b -> Record a b
     f _ = Record

instance (ToCommDisjDict a, ToCommDisjDict b) => ToCommDisjDict (a :@ b) where
  toCDD a = toCDD (fromCDL a) ++ toCDD (fromCDR a)

instance ToCommDisjDict a where
  toCDD _ = []
-}




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
--  TypeXOR (LeftP TypeNotFound) a = a
--  TypeXOR a (RightP TypeNotFound) = a
  TypeXOR TypeNotFound a = a
  TypeXOR a TypeNotFound = a
  TypeXOR a b = TypeNotFound

type family TypeFromRecord (a :: Symbol) b where
  TypeFromRecord a (Record a b) = b
  TypeFromRecord a (b :@ c) = TypeXOR (TypeFromRecord a b) (TypeFromRecord a c)
  TypeFromRecord a  b       = TypeNotFound

{-
data IdentP
data LeftP a
data RightP a

fromLeftP :: LeftP a -> a
fromLeftP = undefined

fromRightP :: RightP a -> a
fromRightP = undefined

type family PathFromRecord a b where
  PathFromRecord a (Record a b) = IdentP
  PathFromRecord a (b :@ c) = TypeXOR (LeftP (PathFromRecord a b)) (RightP (PathFromRecord a c))
  PathFromRecord a  b       = TypeNotFound

data Path = IdentP | LeftP Path | RightP Path

class PathToData a where
  pathToData :: a -> Path

instance PathToData IdentP where
  pathToData _ = IdentP

instance PathToData a => PathToData (LeftP a) where
  pathToData a = LeftP $ pathToData $ fromLeftP a

instance PathToData a => PathToData (RightP a) where
  pathToData a = RightP $ pathToData $ fromRightP a

class PathDataToFunction a where
  pathDataToFunction :: Path -> a

instance PathDataToFunction (a -> Record b a) where
  pathDataToFunction IdentP = Record

instance PathDataToFunction (a -> c) => PathDataToFunction (a -> c :@ d) where
  pathDataToFunction (LeftP a) = CommDisj . Left . pathDataToFunction a

instance PathDataToFunction (a -> e) => PathDataToFunction (a -> (c :@ d) :@ e) where
  pathDataToFunction (RightP a) = CommDisj . Right . pathDataToFunction a
-}

{-
type family PathToFunction a b c d where
  PathToFunction IdentP b c d = (b -> Record c b)
  PathToFunction (Left a) b c d = (b -> PathToFunction a 
-}

{-
class ToCDD a b | a -> b where
  toCDD :: a -> b

instance ToCDD IdentP (a -> Record b a) where
  toCDD _ = Record

instance ToCDD a (a -> c) => ToCDD (LeftP a) (a -> c :@ d) where
  toCDD a = CommDisj . Left . toCDD (fromLeftP a)

instance ToCDD a (a -> d) => ToCDD (RightP a) (a -> c :@ d) where
  toCDD a = CommDisj . Right . toCDD (fromRightP a)
-}

{-
class ToCDD (a :: Symbol) b c where
  toCDD :: Proxy a -> b -> c
  toCDD = undefined

instance (ToCDD a (TypeFromRecord a c) c) => ToCDD "AllSymbols" (Record a b) c
instance (ToCDD a (TypeFromRecord a e) e, ToCDD c (TypeFromRecord c e) e) => ToCDD "AllSymbols" (Record a b :@ Record c d) e

instance ToCDD a b (Record a b) where
  toCDD _ b = Record b

instance ToCDD a b (Record a b :@ c) where
  toCDD _ b = CommDisj $ Left  $ Record b

instance (ToCDD a b c, TypeFromRecord a d ~ TypeNotFound) => ToCDD a b (c :@ (d :@ e)) where
  toCDD a b = CommDisj $ Left  $ toCDD a b

instance (ToCDD a b d, TypeFromRecord a c ~ TypeNotFound) => ToCDD a b (c :@ d) where
  toCDD a b = CommDisj $ Right $ toCDD a b

-}
fromCDD = undefined




{-
condParaM :: (CondBifunctorM t,                            Monad m,                            Fixable a,  Match a (FirstPrototype t) ~ 'True
                , ToCDD "AllSymbols" (FirstPrototype t) a)
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
-}




