{-# LANGUAGE GADTs #-}

{-

https://en.wikipedia.org/wiki/Sequent_calculus
https://en.wikipedia.org/wiki/Hypersequent

-}

module SequentCalculus.Types where

data Side
data Sequent
data NestedSequent
data HyperSequent
data Rule

class RuleSide a where

instance RuleSide Sequent
instance RuleSide NestedSequent
instance RuleSide HyperSequent

data Sqcalc t a where
  Lift  :: t (Sqcalc t Side)                                    -> Sqcalc t Side
  Tack  ::    Sqcalc t Side    -> Sqcalc t Side                 -> Sqcalc t Side
  Sdset ::   [Sqcalc t Side]                                    -> Sqcalc t Side
  Turns ::    Sqcalc t Side    -> Sqcalc t Side                 -> Sqcalc t Sequent
  Sqset ::   [Sqcalc t Sequent]                                 -> Sqcalc t Sequent
  NsBox ::  RuleSide a => Sqcalc t a -> Int                     -> Sqcalc t NestedSequent
  Hyper ::   [Sqcalc t Sequent]                                 -> Sqcalc t HyperSequent
  Infer :: (RuleSide a, RuleSide b) => Sqcalc t a -> Sqcalc t b -> Sqcalc t Rule

infixr 8 `Turns`
infixr 7 `Infer`




