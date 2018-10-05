{-# LANGUAGE GADTs, FlexibleInstances #-}

module SequentCalculus.Catarotoid where

import Catarotoid.Term
import SequentCalculus.Types

instance Indeterminant (Term (Sqcalc Term Side)) where
  va name = Expr (Indet name)

catarotoidRules :: [Sqcalc Term Rule]
catarotoidRules =
  [                 Sdset [va "Γ", va "A", va "B"] `Turns` va "Δ"
    `Infer` ----------------------------------------------------------- tensor left
            Sdset [va "Γ", Lift (va "A" `CMCNJ` va "B")] `Turns` va "Δ"
  ]





