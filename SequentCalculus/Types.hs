{-# LANGUAGE GADTs #-}

module SequentCalculus.Types where

data Side
data Sequent
data Rule

data Sqcalc t a where
  Lift  :: t (Sqcalc t Side)                       -> Sqcalc t Side
  Tack  ::    Sqcalc t Side    -> Sqcalc t Side    -> Sqcalc t Side
  Sdset ::   [Sqcalc t Side]                       -> Sqcalc t Side
  Turns ::    Sqcalc t Side    -> Sqcalc t Side    -> Sqcalc t Sequent
  Sqset ::   [Sqcalc t Sequent]                    -> Sqcalc t Sequent
  Infer ::    Sqcalc t Sequent -> Sqcalc t Sequent -> Sqcalc t Rule

infixr 8 `Turns`
infixr 7 `Infer`




