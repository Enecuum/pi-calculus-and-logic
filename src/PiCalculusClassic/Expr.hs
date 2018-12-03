{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, TypeApplications #-}

module PiCalculusClassic.Expr where

import Control.Morphisms.Prelude
import Data.Proxy
import Data.Dynamic
import Prelude hiding (to)

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
  condBimapM p f j o@(Value a)    | p o = do b <- f (to @"Value" a); return $ Value (fr @"Value" b)
  condBimapM p f j o@(Send a b c) | p o = do d <- f (to @"Name" a); e <- f (to @"Name" b); g <- j (inF c); return $ Send (fr @"Name" d) (fr @"Name" e) (outF g)
  condBimapM p f j o@(Recv a b c) | p o = do d <- f (to @"Name" a); e <- f (to @"Name" b); g <- j (inF c); return $ Recv (fr @"Name" d) (fr @"Name" e) (outF g)
  condBimapM p f j o@(Scop a b)   | p o = do c <- f (to @"Name" a);                        d <- j (inF b); return $ Scop (fr @"Name" c) (outF d)
  condBimapM p f j o@(Comm a b)   | p o = do                               c <- j (inF a); d <- j (inF b); return $ Comm (outF c) (outF d)
  condBimapM p f j o@(Serv a  )   | p o = do                                               b <- j (inF a); return $ Serv $ outF b
  condBimapM p f j o = return $ cast o
   where
    cast :: ExprBF a b -> ExprBF c d
    cast Unit = Unit

test01 :: ExprA
test01 = Scop "test" Unit

test02 :: ExprA
test02 = condBimapP @"Name" (const True) (++"best") id test01











