{-# LANGUAGE GADTs, MultiParamTypeClasses #-}

module MorphismCalculus where

data Composition a b
data Ident
data Zero
data Empty
data Union a b

class Morphism a where

class Set a where

class Unionable a b where

instance Set Empty where

instance Set (Category a) where

instance (Morphism a, Morphism b) => Morphism (Composition a b) where
instance Morphism Ident where
instance Morphism Zero where

data Category a where
 Conjunction :: Category a -> Category a -> Category a
 Disjunction :: Category a -> Category a -> Category a
 Product     :: Category a -> Category a -> Category a
 CoProduct   :: Category a -> Category a -> Category a
 Category    :: Morphism m => { morphism :: Category m, domain :: Category a, codomain :: Category a } -> Category b
 Composition :: (Morphism a, Morphism b) => Category a -> Category b -> Category (Composition a b)
 Ident       :: Category Ident
 Zero        :: Category Zero
 Empty       :: Category Empty
 Union       :: (Set a, Set b, Unionable a b) => Category a -> Category b -> Category (Union a b)
