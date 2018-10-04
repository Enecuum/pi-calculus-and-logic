module PiCalculusLogic.Expr where

import Catarotoid.Pattern

data Expr
   = Unit
   | Send { chanToPipe :: [Pattern], nameToSend :: [Pattern] }
   | Recv { chanToWait :: [Pattern], nameToBind :: [Pattern] }
   | Scop { newChannelName :: [Pattern] }
   | Expr `Comm` Expr
   | Expr `Sepa` Expr
   | Expr `Case` Expr
   | Expr `Nond` Expr
   | Serv Expr











