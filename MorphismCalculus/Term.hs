{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances #-}

module MorphismCalculus where

data Composition a b
data Ident
data Empty
data Union a b

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
 Category    :: Morphism m => { morphism :: Category m, domain :: Category a, codomain :: Category a } -> Category b
 Composition :: (Morphism a, Morphism b) => Category a -> Category b -> Category (Composition a b)
 Ident       :: Category Ident
 Empty       :: Category Empty
 Union       :: Unionable a b => Category a -> Category b -> Category (Union a b)
