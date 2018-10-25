module PiCalculusClassic.Expr where

import Catarotoid.Pattern

data Expr
   = Unit
   | Send { chanToPipe :: Pattern, nameToSend :: Pattern, process :: Expr }
   | Recv { chanToWait :: Pattern, nameToBind :: Pattern, process :: Expr }
   | Scop { newChannelName :: Pattern, process :: Expr }
   | Expr `Comm` Expr
   | Serv Expr











