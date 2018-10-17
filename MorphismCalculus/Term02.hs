module MorphismCalculus.Term02 where

data Term a
   = Idnt
   | Empt
   | Conj (Term a) (Term a)
   | Disj (Term a) (Term a)
   | Prod (Term a) (Term a)
   | Mdsj (Term a) (Term a)
   | Comp (Term a) (Term a)
   | Unio (Term a) (Term a)
   | Name [String]
   | Morp { morphism :: Term a, domain :: Term a, codomain :: Term a }



