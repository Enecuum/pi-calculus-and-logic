{-# LANGUAGE GADTs #-}

module SequentCalculus.Types where

data Side
data Sequent
data NestedSequent
data Rule

class RuleSide a where

instance RuleSide Sequent
instance RuleSide NestedSequent

data Sqcalc t a where
  Lift  :: t (Sqcalc t Side)                                    -> Sqcalc t Side
  Tack  ::    Sqcalc t Side    -> Sqcalc t Side                 -> Sqcalc t Side
  Sdset ::   [Sqcalc t Side]                                    -> Sqcalc t Side
  Turns ::    Sqcalc t Side    -> Sqcalc t Side                 -> Sqcalc t Sequent
  Sqset ::   [Sqcalc t Sequent]                                 -> Sqcalc t Sequent
  NsBox ::  RuleSide a => Sqcalc t a -> Int                     -> Sqcalc t NestedSequent
  Infer :: (RuleSide a, RuleSide b) => Sqcalc t a -> Sqcalc t b -> Sqcalc t Rule

infixr 8 `Turns`
infixr 7 `Infer`




