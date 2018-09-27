
import Control.Concurrent.STM.TVar

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


 | Atom Descr

data Descr = Descr { path :: [Maybe (Either String Integer)], isReduced :: TVar (Maybe Integer) }







