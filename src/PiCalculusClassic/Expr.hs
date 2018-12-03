{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-}

module PiCalculusClassic.Expr where

import Control.Morphisms.Prelude
import Data.Proxy
import Data.Dynamic

name :: Proxy "Name"
name =  Proxy

value :: Proxy "Value"
value =  Proxy

type R a b = Record a b
type T a b = TypeFromRecord a b

type Expr = FixF ExprF
type ExprA = ExprF Expr
type ExprF = ExprBF (R "Value" Integer :@ R "Name" String)

data ExprBF a b
   = Unit
   | Value (T "Value" a)
   | Send { chanToPipe :: (T "Name" a), nameToSend :: (T "Name" a), process :: OutF b }
   | Recv { chanToWait :: (T "Name" a), nameToBind :: (T "Name" a), process :: OutF b }
   | Scop { newChannelName :: (T "Name" a), process :: OutF b }
   | OutF b `Comm` OutF b
   | Serv (OutF b)

deriving instance Show Expr
deriving instance Show ExprA

instance CondBifunctorM ExprBF where
  type FirstPrototype ExprBF = (R "Value" AnyType :@ R "Name" AnyType)
  condBimapM p f j o@(Scop a b) | p o = do c <- f (toCDD name a); d <- j (inF b); return $ Scop (fromCDD name c) (outF d)
  condBimapM p f j o@(Comm a b) | p o = do c <- j (inF        a); d <- j (inF b); return $ Comm (outF c) (outF d)
  condBimapM p f j o@(Serv a  ) | p o = do b <- j (inF        a);                 return $ Serv $ outF b
  condBimapM p f j o = return $ cast o
   where
    cast :: ExprBF a b -> ExprBF c d
    cast Unit = Unit

test01 :: ExprA
test01 = Scop "test" Unit

test02 :: ExprA
test02 = condBimap (const True) (\(CommDisj (Right (Record a))) -> CommDisj (Right (Record (a++"best")))) id test01











