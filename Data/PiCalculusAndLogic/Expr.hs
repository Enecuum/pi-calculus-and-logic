module Data.PiCalculysAndLogic.Expr where

data Expr = Empt | Fail
          | Inhe | Star | Mark
          | Text  String
          | Abbr [String]
          | Chan [String]
          | Blck [String] (Either String Expr)
          | Send { chanToPipe :: Expr, nameToSend :: Expr }
          | Wait { chanToWait :: Expr, nameToBind :: Expr }
          | OptW { chanToWait :: Expr }
          | OptF  Expr
          | Comm [Expr]
          | Ordr [Expr]
          | Serv  Expr
          | Log2  Expr

 deriving (Eq,Ord,Read,Show)










