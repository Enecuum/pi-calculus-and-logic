{-# LANGUAGE GADTs #-}

module SequentCalculus.ClassicLogic where

import SequentCalculus.Types hiding (Indet)

data Classic a where
  Elem  :: a                      -> Classic a
  Indet :: String                 -> Classic a
  Conj  :: Classic a -> Classic a -> Classic a
  Disj  :: Classic a -> Classic a -> Classic a
  Not   :: Classic a              -> Classic a

classicRules :: [Sqcalc Classic Rule]
classicRules =
  [         Sdset [Lift (Indet "Γ"), Lift (Indet "A" `Conj` Indet "B")] `Turns` Lift (Indet "Δ")
    `Infer` -------------------------------------------------------------------------------------
            Sdset [Lift (Indet "Γ"), Lift (Indet "A"), Lift (Indet "B")] `Turns` Lift (Indet "Δ")
  ]



