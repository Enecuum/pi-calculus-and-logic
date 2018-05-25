module Data.PiCalculysAndLogic.Expr where

data Expr = Empt | Fail
          | Inhe | Star | Mark
          | Chan [String]
          | Send { chanToPipe :: Expr, nameToSend :: Expr }
          | Wait { chanToWait :: Expr, nameToBind :: Expr }
          | OptW { chanToWait :: Expr }
          | OptF  Expr
          | Comm [Expr]
          | Ordr [Expr]
          | Serv  Expr
          | Log2  Expr

 deriving (Eq,Ord)










