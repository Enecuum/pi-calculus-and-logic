module Catarotoid.Pattern where

import Control.Concurrent.STM.TVar
import System.IO.Unsafe
import System.Random
import Data.Maybe
import Data.Functor.Identity
import Control.Monad.State.Lazy
import Data.Tuple

data Pattern = ConstName String | ConstId Integer | UniqueId Integer | UniqueWildcard Integer
 deriving (Eq)

data Descr   = Descr { path :: [Pattern], isReduced :: TVar (Maybe Integer) }
 deriving (Eq)

instance Show Descr where
  show (Descr a b) = concat $ map (':':) $ map f a
   where
    f (UniqueWildcard _) = "*"
    f (ConstName a) = a
    f (UniqueId a) = "uid" ++ show a
    f (ConstId a) = "id" ++ show a








