module Catarotoid.UniqueShare where

import Control.Concurrent.STM.TVar
import System.IO.Unsafe
import System.Random
import Data.Maybe
import Data.Functor.Identity
import Control.Monad.State.Lazy
import Data.Tuple

data UniqueShare = USH Integer

instance Show UniqueShare where
  show (USH a) = "#" ++ show (a `mod` 99)

instance Eq UniqueShare where
  USH a == USH b = a == b

newUniqueShare :: IO UniqueShare
newUniqueShare = do
  a <- randomRIO (2^80,2^90)
  return $ USH a
