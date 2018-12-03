{-# LANGUAGE DataKinds, TypeOperators #-}

module PiCalculusClassic.Expr where

import Control.Morphisms.Prelude

type R a b = Record a b
type T a b = TypeFromRecord a b

type Expr = FixF ExprF
type ExprF = ExprBF (R "Value" Integer :@ R "Name" String)

data ExprBF a b
   = Unit
   | Value (T "Value" a)
   | Send { chanToPipe :: (T "Name" a), nameToSend :: (T "Name" a), process :: OutF b }
   | Recv { chanToWait :: (T "Name" a), nameToBind :: (T "Name" a), process :: OutF b }
   | Scop { newChannelName :: (T "Name" a), process :: OutF b }
   | OutF b `Comm` OutF b
   | Serv (OutF b)

instance CondBifunctorM ExprBF where
  -- condBimapM p f j o@(Scop a b) | p o = do c <- f      a ; d <- j (inF b); return $ Comm       c  (outF d)
  condBimapM p f j o@(Comm a b) | p o = do c <- j (inF a); d <- j (inF b); return $ Comm (outF c) (outF d)
  condBimapM p f j o@(Serv a  ) | p o = do b <- j (inF a);                 return $ Serv $ outF b
  condBimapM p f j o = return $ cast o
   where
    cast :: ExprBF a b -> ExprBF c d
    cast Unit = Unit











