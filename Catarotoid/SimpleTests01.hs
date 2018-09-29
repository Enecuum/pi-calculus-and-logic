module Catarotoid.SimpleTests01 where

import Control.Concurrent.STM.TVar
import System.IO.Unsafe
import System.Random
import Data.Maybe
import Data.Functor.Identity
import Control.Monad.State.Lazy
import Data.Tuple

import Catarotoid.Term
import Catarotoid.Pattern
import Catarotoid.FocusRotation
import Catarotoid.FocusUp
import Catarotoid.Reductions
import Catarotoid.MiscTools
import Catarotoid.GraphViz
import Catarotoid.Morphisms

at x = unsafePerformIO $ do
  tv <- newTVarIO Nothing
  return $ Atom $ Descr [ConstName x] tv

test01 = [(at "q" `CMCNJ` (Focus (at "w") `CMCNJ` at "e"), at "r" `CMCNJ` at "t")]
test02 = [(at "q" `CMCNJ` (Focus (at "w") `CMCNJ` Focus (at "e")), at "r" `CMCNJ` at "t")]

test03 = [(MROOT, ((at "a" `CMCNJ` INVRT (at "b")) `NMCNJ` at "c") `CMCNJ` at "d")]
test04 = [(MROOT, ((Focus (at "a") `CMCNJ` INVRT (at "b")) `NMCNJ` at "c") `CMCNJ` at "d")]
test05 = [(MROOT, ((Focus (at "a") `CMCNJ` INVRT (at "b")) `NMCNJ` Focus (at "c")) `CMCNJ` at "d")]
test06 = [(MROOT, ((at "a" `CMCNJ` INVRT (at "b")) `NMCNJ` at "c") `CMCNJ` Focus (at "d"))]






