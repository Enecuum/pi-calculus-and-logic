module PiCalculusLogic.Expr where

import Catarotoid.Term
import Catarotoid.Pattern

data Expr
   = Term (Term Expr)
   | Send { chanToPipe :: [Pattern], nameToSend :: [Pattern] }
   | Recv { chanToWait :: [Pattern], nameToBind :: [Pattern] }
   | Scop { newChannelName :: [Pattern] }











