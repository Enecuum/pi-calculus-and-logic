module Catarotoid.FocusRotation where

import Control.Concurrent.STM.TVar
import System.IO.Unsafe
import System.Random
import Data.Maybe
import Data.Functor.Identity
import Control.Monad.State.Lazy
import Data.Tuple

import Catarotoid.Term
import Catarotoid.UniqueShare

rotateFocus [] = []

rotateFocus [(Focus a,b)] = rotateFocus [(a,b)]

rotateFocus o@[(a,b)] | isJust c = case d of
  (Focus g, Focus h) -> unsafePerformIO $ do
    ushH <- newUniqueShare
    let shH = Share ushH
    let rot01 = rotateFocus [(g, e b $ f shH)]
    let rot02 = rotateFocus [(h, shH)] -- term h equal to shH in sense of connected shares
    return $ rot01 ++ rot02
  (Focus g, h) -> rotateFocus [(g, e b $ f h)]
  (g, Focus h) -> rotateFocus [(h, e (f g) b)]
  _ -> o
 where
  c = toBinop a
  Just (d,(e,f)) = c

rotateFocus (a:b:c) = rotateFocus [a] ++ rotateFocus (b:c)

rotateFocus a = a






