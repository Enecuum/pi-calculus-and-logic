{-# LANGUAGE GADTs, FlexibleInstances #-}

{-

https://en.wikipedia.org/wiki/Sequent_calculus
https://en.wikipedia.org/wiki/Hypersequent
https://en.wikipedia.org/wiki/Nested_sequent_calculus

-}

module SequentCalculus.Types where

data Side
data Sequent
data HyperSequent
data Rule

class RuleSide a where

instance RuleSide Sequent
instance RuleSide HyperSequent

data Sqcalc t a where
  Indet ::    String                                            -> Sqcalc t Side
  Lift  :: t (Sqcalc t Side)                                    -> Sqcalc t Side
  Tack  ::    Sqcalc t Side    -> Sqcalc t Side                 -> Sqcalc t Side
  Sdset ::   [Sqcalc t Side]                                    -> Sqcalc t Side
  Turns ::    Sqcalc t Side    -> Sqcalc t Side                 -> Sqcalc t Sequent
  Sqset ::   [Sqcalc t Sequent]                                 -> Sqcalc t Sequent
  Sqhyp ::   [Sqcalc t Sequent]                                 -> Sqcalc t HyperSequent
  Infer :: (RuleSide a, RuleSide b) => Sqcalc t a -> Sqcalc t b -> Sqcalc t Rule

class Indeterminant a where
  va :: String -> a

instance Indeterminant (Sqcalc t Side) where
  va name = Indet name

infixr 8 `Turns`
infixr 7 `Infer`




