{-# LANGUAGE DataKinds #-}

module Data.PiCalculusAndLogic.Math where

import Data.Numbers.Primes
import Data.Vector.Fixed.Boxed
import Data.Ratio
import Math.NumberTheory.Primes.Factorisation

data Value = Invert Value | Prime Integer | PermVect (Vec 64 (Ratio Integer))
           | Monoid [Value]
           | Multip [Value]

-- randomAtom = do
