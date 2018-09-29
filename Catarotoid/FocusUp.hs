module Catarotoid.FocusUp where

import Control.Concurrent.STM.TVar
import System.IO.Unsafe
import System.Random
import Data.Maybe
import Data.Functor.Identity
import Control.Monad.State.Lazy
import Data.Tuple

import Catarotoid.Term
import Catarotoid.Morphisms

isFocusExistInTermOp :: Term -> Bool
isFocusExistInTermOp a = snd $ runState (termMapM p f a) False
 where
  p (Focus _) = False
  p        _  = True
  f :: Term -> State Bool Term
  f o@(Focus _) = put True >> return o
  f o           =             return o

focusAll a = termCata p f a
 where
  p _ = True
  f o@(Share _) =       o
  f o@(Focus _) =       o
  f o           = Focus o

class FocusUp a where
  focusUp :: a -> a

instance FocusUp Term where
  focusUp = termCata (const True) f
   where
    f a | isFocusExistInTermOp a = Focus a
    f a = a

focup01 a = map (tupleMap focusUp) a
focup02 a = map (tupleMap focusAll) a

