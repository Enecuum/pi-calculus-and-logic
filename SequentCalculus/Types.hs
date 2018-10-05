module SequentCalculus.Types where

data Sqcalc a where


  Tack  ::  Sqcalc Side    -> Sqcalc Side    -> Sqcalc Side
  Sdset :: [Sqcalc Side]                     -> Sqcalc Side
  Turns ::  Sqcalc Side    -> Sqcalc Side    -> Sqcalc Sequent
  Sqset :: [Sqcalc Sequent]                  -> Sqcalc Sequent
  Infer ::  Sqcalc Sequent -> Sqcalc Sequent -> Sqcalc Rule



