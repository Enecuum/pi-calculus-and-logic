{-# LANGUAGE TypeFamilies, GADTs, FlexibleInstances #-}

module MathForLinearLogic.Algebra where

import Data.Word
import qualified Prelude as P
import Prelude hiding (Integer,Int,Rational)

data NATURAL
data INTEGER
data RATIONAL

type family Add a b where

type family Mul a b where

type family Div a b where

type family Inv a where

type family Log a where

type family Exp a where

type family Neg a where

type family Pow a b where

data Term a where
  Natural  :: P.Integer        -> Term NATURAL
  Integer  :: Integer  a => Term a -> Term INTEGER
  Rational :: Rational a => Term a -> Term RATIONAL
  Add :: Term a -> Term b -> Term (Add a b)
  Mul :: Term a -> Term b -> Term (Mul a b)
  Div :: Term a -> Term b -> Term (Div a b)
  Pow :: Term a -> Term b -> Term (Pow a b)
  Log :: Term a           -> Term (Log a)
  Exp :: Term a           -> Term (Exp a)
  Inv :: Term a           -> Term (Inv a)
  Neg :: Term a           -> Term (Neg a)

instance Num (Term NATURAL) where
  Natural a + Natural b = Natural (a+b)
  Natural a * Natural b = Natural (a*b)

class Integer a where

class Rational a where


