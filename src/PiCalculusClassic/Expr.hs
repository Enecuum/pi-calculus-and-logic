{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, TypeApplications #-}

module PiCalculusClassic.Expr where

import Control.Morphisms.Prelude
import Data.Proxy
import Data.Dynamic
import Prelude hiding (to)
import GHC.TypeLits

type R a b = Record a b
type T a b = TypeFromRecord a b

type Expr = FixF ExprF
type ExprA = ExprF Expr
--type ExprF = AddFixBF "Test" FixType ExprBF (R "Value" Integer :@ R "Name" String)
type Args01 = R "Value" Integer :@ (R "Test" Int :@ R "Name" String)
type Args02 = Args01 :@ R "Test" Int
type ExprF = ExprBF ( Args01 ) -- :@ R "Test" (FixF (FlipBF "Test" ExprBF Args01)) )

test0001 :: T "Test" Args02
test0001 = undefined

data ExprBF a b
   = Unit
   | Value (T "Value" a)
   | Send { chanToPipe :: (T "Name" a), nameToSend :: (T "Name" a), process :: OutF b }
   | Recv { chanToWait :: (T "Name" a), nameToBind :: (T "Name" a), process :: OutF b }
   | Scop { newChannelName :: (T "Name" a), process :: OutF b }
   | OutF b `Comm` OutF b
   | Serv (OutF b)
   | Test (OutF (T "Test" a))

deriving instance Show Expr
deriving instance Show ExprA

instance CondBifunctorM ExprBF where
  type FirstPrototype ExprBF = R "Value" AnyType :@ R "Name" AnyType :@ R "Test" AnyType
  condBimapM p f j o@(Value a)    | p o = do b <- f (to @"Value" a);                                        return $ Value (fr @"Value" b)
  condBimapM p f j o@(Send a b c) | p o = do d <- f (to @"Name"  a); e <- f (to @"Name" b); g <- j (inF c); return $ Send  (fr @"Name"  d) (fr @"Name" e) (outF g)
  condBimapM p f j o@(Recv a b c) | p o = do d <- f (to @"Name"  a); e <- f (to @"Name" b); g <- j (inF c); return $ Recv  (fr @"Name"  d) (fr @"Name" e) (outF g)
  condBimapM p f j o@(Scop a b)   | p o = do c <- f (to @"Name"  a);                        d <- j (inF b); return $ Scop  (fr @"Name"  c) (outF d)
  condBimapM p f j o@(Comm a b)   | p o = do                                c <- j (inF a); d <- j (inF b); return $ Comm  (outF c) (outF d)
  condBimapM p f j o@(Serv a  )   | p o = do                                                b <- j (inF a); return $ Serv $ outF b
  condBimapM p f j o = return $ cast o
   where
    cast :: ExprBF a b -> ExprBF c d
    cast Unit = Unit

--instance CondBifunctorM (FlipBF (a :: Symbol) ExprBF) where
--  type FirstPrototype (FlipBF a ExprBF) = FirstPrototype ExprBF

test01 :: ExprA
test01 = Scop "test" Unit

test02 :: ExprA
test02 = condBimapP @"Name" (const True) (++"best") id test01
--test02 = condBimap (const True) id id test01







---------------------------



 
