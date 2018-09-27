
import Control.Concurrent.STM.TVar
import System.IO.Unsafe

data Term
 = MROOT
 | AUNIT
 | MUNIT
 | Term `CMCNJ` Term
 | Term `CMDSJ` Term
 | Term `NMCNJ` Term
 | Term `CADSJ` Term
 | Term `CACNJ` Term
 | INVRT Term
 | NEGAT Term
 | OFCRS Term
 | WHYNT Term

 | Atom Descr

 deriving (Show,Eq)

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

at x = unsafePerformIO $ do
  tv <- newTVarIO Nothing
  return $ Atom $ Descr [ConstName x] tv

{-


 конструкции

 a * b * a^-1
 c^-1 * d * c


 -}







