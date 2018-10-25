module PiCalculusLogic.Term where

type Name = Integer

data Term a
 = Send { toChan   :: Name, nameToSend :: Maybe Name }
 | Recv { fromChan :: Name, nameToBind :: Maybe Name }
 | Term a `Proc` Term a
 | Term a `Mult` Term a
 | Term a `Plus` Term a
 | Term a `Case` Term a
 | Term a `Isol` Term a
 | Nat   Integer
 | New Name
 | Log  (Term a)
 | Exp  (Term a)
 | Neg  (Term a)
 | Inv  (Term a)
 | Serv (Term a)
 | Indet Int
 | Const String

identities =
 [ ( Indet 10 `Mult` Indet 20, Indet 20 `Mult` Indet 10 )
 , ( Indet 10 `Case` Indet 20, Inv (Inv (Indet 10) `Plus` Inv (Indet 20)) )
 , ( Indet 10 `Isol` Indet 20, Exp (Log (Indet 10) `Case` Log (Indet 20)) )
 -- , Const "Top", Nat 0 
 ]

--a & b = Inve (Inve a `Plus` Inve b)


{-

a `Mult` 1 = a

a `Mult` Inve a = 1 (Success)

a `Mult` Infinity = Infinity (Fail) in some cases

a `Isol` Inve a = Infinity






-}

offer  a b = Inv (Recv a b)
accept a b = Inv (Send a b)

