{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, KindSignatures, DataKinds, FlexibleContexts, UndecidableInstances, FunctionalDependencies, TypeFamilies #-}

module MorphismCalculus where

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

instance   IsValidCategory (Category (m,a,b)) where
instance   IsValidCategory (Category Empty) where

instance ( IsValidCategory (Category a)
         , IsValidCategory (Category b)
         , IsValidSetOfMorphisms a b
         ) => IsValidCategory (Category (Union a b)) where

instance ( IsValidMorphism a b c ~ 'True
         , IsValidMorphism d e f ~ 'True
         , IsValidPairOfMorphisms a b c d e f ~ 'True) => IsValidSetOfMorphisms (a,b,c) (d,e,f) where

instance ( IsValidCategory (Category (Union c d))
         , IsValidCategory (Category (Union a c))
         , IsValidCategory (Category (Union a d))
         ) => IsValidSetOfMorphisms a (Union c d) where

type family IsValidMorphism a b c where
  IsValidMorphism Ident a a = 'True
  IsValidMorphism Ident a b = 'False
  IsValidMorphism a b c = 'True

type family IsValidPairOfMorphisms a b c d e f where
  IsValidPairOfMorphisms a b c a b c = 'True
  IsValidPairOfMorphisms a b c a d e = 'False
  IsValidPairOfMorphisms a b c d e f = 'True


test02 :: Category (Union (Symbol "a", Symbol "b", Symbol "c") (Symbol "a", Symbol "b", Symbol "c"))
test02 = undefined

test03 :: IsValidCategory a => a -> a
test03 a = a


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

data Category a where
 Conjunction :: Category a -> Category a -> Category a
 Disjunction :: Category a -> Category a -> Category a
 Product     :: Category a -> Category a -> Category a
 CoProduct   :: Category a -> Category a -> Category a
 Category    :: Morphism m => { morphism :: Category m, domain :: Category a, codomain :: Category b } -> Category (m,a,b)
 Composition :: (Morphism a, Morphism b) => Category a -> Category b -> Category (Composition a b)
 Symbol      :: KnownSymbol a => Category (Symbol a)
 Ident       :: Category Ident
 Empty       :: Category Empty
 Union       :: Unionable a b => Category a -> Category b -> Category (Union a b)





