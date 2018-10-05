{-# LANGUAGE GADTs, FlexibleInstances #-}

module SequentCalculus.Catarotoid where

import Catarotoid.Term
import SequentCalculus.Types

instance Indeterminant (Term (Sqcalc Term Side)) where
  va name = Expr (Indet name)

rulesLinearLogic :: [Sqcalc Term Rule]
rulesLinearLogic =


  [                 Sdset [va "Γ", va "A", va "B"] `Turns` va "Δ"
    `Infer` ----------------------------------------------------------- tensor left
            Sdset [va "Γ", Lift (va "A" `CMCNJ` va "B")] `Turns` va "Δ"



  ,         Sqset [va "Γ" `Turns` Sdset [va "A", va "Δ"], va "Γ'" `Turns` Sdset [va "B", va "Δ'"]]
    `Infer` -------------------------------------------------------------------------------------- tensor right
            Sdset [va "Γ", va "Γ'"] `Turns` Sdset [Lift (va "A" `CMCNJ` va "B"), va "Δ", va "Δ'"]



  ]





