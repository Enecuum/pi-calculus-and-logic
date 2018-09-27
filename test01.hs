
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

data Descr = Descr { path :: [Maybe (Either String Integer)], isReduced :: TVar (Maybe Integer) }
 deriving (Eq)

instance Show Descr where
  show (Descr a b) = concat $ map (':':) $ map f a
   where
    f Nothing = "*"
    f (Just (Left a)) = a
    f (Just (Right a)) = show a

at x = unsafePerformIO $ do
  tv <- newTVarIO Nothing
  return $ Atom $ Descr [Just $ Left x] tv







