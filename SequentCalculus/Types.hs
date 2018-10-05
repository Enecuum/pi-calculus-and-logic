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

data Classic a where
  Elem  :: a                      -> Classic a
  Indet :: String                 -> Classic a
  Conj  :: Classic a -> Classic a -> Classic a
  Disj  :: Classic a -> Classic a -> Classic a
  Not   :: Classic a              -> Classic a

infixr 8 `Turns`
infixr 7 `Infer`

classicRules :: [Sqcalc Classic Rule]
classicRules =
  [         Sdset [Lift (Indet "Γ"), Lift (Indet "A" `Conj` Indet "B")] `Turns` Lift (Indet "Δ")
    `Infer` -------------------------------------------------------------------------------------
            Sdset [Lift (Indet "Γ"), Lift (Indet "A"), Lift (Indet "B")] `Turns` Lift (Indet "Δ")
  ]



