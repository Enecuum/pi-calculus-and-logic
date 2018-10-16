{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, KindSignatures, DataKinds, FlexibleContexts, UndecidableInstances, FunctionalDependencies #-}

module MorphismCalculus where

import GHC.TypeLits hiding (Symbol)
import qualified GHC.TypeLits as T

data Composition a b
data Ident
data Empty
data Union a b
data Symbol (a :: T.Symbol)

class IsValidCategory a where

class IsValidSetOfMorphisms a b where

class IsValidPairOfMorphisms a b c d e f | a -> b, a -> c, d -> e, d -> f where

instance   IsValidCategory (Category (m,a,b)) where
instance   IsValidCategory (Category Empty) where

instance ( IsValidCategory (Category a)
         , IsValidCategory (Category b)
         , IsValidSetOfMorphisms a b
         ) => IsValidCategory (Category (Union a b)) where

instance IsValidPairOfMorphisms a b c d e f => IsValidSetOfMorphisms (a,b,c) (d,e,f) where

instance ( IsValidCategory (Category (Union c d))
         , IsValidCategory (Category (Union a c))
         , IsValidCategory (Category (Union a d))
         ) => IsValidSetOfMorphisms a (Union c d) where


--instance (IsValidCategory (Category a), IsValidCategory (Category (Union c d))) => IsValidSetOfMorphisms a (Union c d) where

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




