module Catarotoid.Reductions where

import Control.Concurrent.STM.TVar
import System.IO.Unsafe
import System.Random
import Data.Maybe
import Data.Functor.Identity
import Control.Monad.State.Lazy
import Data.Tuple

import Catarotoid.Term
import Catarotoid.MiscTools
import Catarotoid.Morphisms

dupReduce (INVRT (INVRT a)) = a
dupReduce (NEGAT (NEGAT a)) = a
dupReduce a = a

--atomInvNorm

dupReduceNet = map (tupleMap (termCata (const True) dupReduce))



