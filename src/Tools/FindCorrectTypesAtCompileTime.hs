module Tools.FindCorrectTypesAtCompileTime (detectNat,findLift) where

import Language.Haskell.TH
import Text.Regex.Posix
import Data.List
import Data.Dynamic
import Data.Typeable
import Data.Char
import Language.Haskell.Meta.Parse
import Data.Typeable
import System.Random
import Control.Monad.IO.Class
--import Debug.Trace.LocationTH
--import Language.Haskell.TH.Syntax (location, Loc(..), Q, Exp, lift)
--import Language.Haskell.TH.Syntax (location,Lift,lift,QExp)
import Language.Haskell.TH.Syntax
import Control.Exception
import Data.Hashable

findCorrectTypesAtCompileTime :: [a] -> DecsQ
findCorrectTypesAtCompileTime a = foldr (\a b -> seq a b) (return []) a

detectNat :: TypeQ
detectNat = do
  a <- location
  let b = hash $ show a
  c <- liftIO $ randomRIO (0,20 :: Integer)
  liftIO $ print ("detectNat",b,c)
  return $ LitT $ NumTyLit c

findLift :: Lift a => a -> ExpQ
findLift a = do
  (error "kuku") `qRecover` (lift a)

