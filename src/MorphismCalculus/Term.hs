{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, KindSignatures, DataKinds, FlexibleContexts, UndecidableInstances, FunctionalDependencies, TypeFamilies, TypeOperators #-}

module MorphismCalculus.Term where

import GHC.TypeLits hiding (Symbol)
import qualified GHC.TypeLits as T
import Data.Bool

data Composition a b
data Ident
data Empty
data Union a b
data Symbol (a :: T.Symbol)

class IsValidCategory a where

class IsValidSetOfMorphisms a b where

instance   IsValidCategory (Term (Ident,a,a)) where
instance   IsValidCategory (Term Empty) where

instance ( ( GetListOfObjects (Union a b) `SubstractListSet` GetListOfIdentedObjects (Union a b) ) ~ ()
         , IsValidSetOfMorphisms a b
         ) => IsValidCategory (Term (Union a b)) where

instance ( IsValidMorphism a b c ~ 'True
         , IsValidMorphism d e f ~ 'True
         , IsValidPairOfMorphisms a b c d e f ~ 'True) => IsValidSetOfMorphisms (a,b,c) (d,e,f) where

instance ( IsValidSetOfMorphisms c d
         , IsValidSetOfMorphisms a c
         , IsValidSetOfMorphisms a d
         ) => IsValidSetOfMorphisms a (Union c d) where

instance ( IsValidSetOfMorphisms a b
         , IsValidSetOfMorphisms a c
         , IsValidSetOfMorphisms b c
         ) => IsValidSetOfMorphisms (Union a b) c where

data a :. b

infixr :.

type family a `SubstractListSet` b where
  SubstractListSet () a = ()
  SubstractListSet a () = a
  SubstractListSet (a :. b) (a :. c) = SubstractListSet b (a :. c)
  SubstractListSet a (b :. c) = SubstractListSet a c `SubstractListSet` (b :. ())

type family GetListOfIdentedObjects a where
  GetListOfIdentedObjects (Ident,a,a) = a :. ()
  GetListOfIdentedObjects (Union a b) = UnionLists (GetListOfIdentedObjects a) (GetListOfIdentedObjects b)
  GetListOfIdentedObjects a = ()

type family GetListOfObjects a where
  GetListOfObjects (a,b,b) = b :. ()
  GetListOfObjects (a,b,c) = b :. c :. ()
  GetListOfObjects (Union a b) = UnionLists (GetListOfObjects a) (GetListOfObjects b)
  GetListOfObjects a = ()

type family UnionLists a b where
  UnionLists a () = a
  UnionLists () a = a
  UnionLists (a :. b) (a :. d) = a :. UnionLists b d
  UnionLists (a :. b) (c :. d) = a :. c :. UnionLists b d

type family IdentExist a b where
  IdentExist a (Ident,a,a) = 'True
  IdentExist a (Union b c) = IdentExist a b `Or` IdentExist a c
  IdentExist a b = 'False

type family a `Or` b where
  Or 'True a = 'True
  Or a 'True = 'True
  Or a a     = 'False

type family IsValidMorphism a b c where
  IsValidMorphism Ident a a = 'True
  IsValidMorphism Ident a b = 'False
  IsValidMorphism a b c = 'True

type family IsValidPairOfMorphisms a b c d e f where
  IsValidPairOfMorphisms a b c a b c = 'True
  IsValidPairOfMorphisms Ident a a Ident b b = 'True
  IsValidPairOfMorphisms a b c a d e = 'False
  IsValidPairOfMorphisms a b c d e f = 'True


test01 :: Term (Ident, Symbol "b", Symbol "b")
test01 = undefined

test02 :: Term ((Symbol "a", Symbol "b", Symbol "b") `Union` (Ident, Symbol "b", Symbol "b"))
test02 = undefined

test04 :: Term ((Symbol "a", Symbol "b", Symbol "c") `Union` (Ident, Symbol "b", Symbol "b") `Union` (Ident, Symbol "c", Symbol "c"))
test04 = undefined

test03 :: IsValidCategory a => a -> a
test03 a = a

test05 = test03 test01
test06 = test03 test02
test07 = test03 test04


class Morphism a where

class Set a where

class Unionable a b where

instance Unionable Empty a where
instance Unionable a a where

instance (Unionable a b, Unionable a c) => Unionable a (Union b c) where
instance (Unionable a c, Unionable b c) => Unionable (Union a b) c where

instance (Morphism a, Morphism b) => Morphism (Composition a b) where
instance Morphism Ident where
instance Morphism Empty where

data Term a where
 Conjunction :: Term a -> Term a -> Term a
 Disjunction :: Term a -> Term a -> Term a
 Product     :: Term a -> Term a -> Term a
 CoProduct   :: Term a -> Term a -> Term a
 Morphism    :: Morphism m => { morphism :: Term m, domain :: Term a, codomain :: Term b } -> Term (m,a,b)
 Composition :: (Morphism a, Morphism b) => Term a -> Term b -> Term (Composition a b)
 Symbol      :: KnownSymbol a => Term (Symbol a)
 Ident       :: Term Ident
 Empty       :: Term Empty
 Union       :: Unionable a b => Term a -> Term b -> Term (Union a b)
 -- Tagged      :: SetOfTags b => Term a -> Term b -> Term (Tagged a b)





