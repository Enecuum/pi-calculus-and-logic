{-# LANGUAGE GADTs #-}

module MorphismCalculus where

class Morphism a where

data Category a where
 Conjunction :: Category a -> Category a -> Category a
 Disjunction :: Category a -> Category a -> Category a
 Product     :: Category a -> Category a -> Category a
 CoProduct   :: Category a -> Category a -> Category a
 Category    :: Morphism m => { morphism :: Category m, domain :: Category a, codomain :: Category a } -> Category b
