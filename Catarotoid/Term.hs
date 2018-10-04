module Catarotoid.Term where

import Control.Concurrent.STM.TVar
import System.IO.Unsafe
import System.Random
import Data.Maybe
import Data.Functor.Identity
import Control.Monad.State.Lazy
import Data.Tuple

import Catarotoid.UniqueShare
import Catarotoid.Pattern

data Term a
 = MCSOLVE -- Multiplicative conjunction solve of top formula, expresion must be equal to multiplicative conjunction unit for success
 | ADUNIT -- Additive disjunction unit, a+0 == a
 | ACUNIT -- Additive conjunction unit, a&⊤ == a
 | MCUNIT -- Multiplicative conjunction unit, a*1 == a, or for example matrix * identitiy matrix for non commutative case
 | MDUNIT -- Multiplicative disjunction unit, a⅋⊥ == a
 | Term a `CMCNJ` Term a -- Commutative Multiplicative Conjunction
 | Term a `CMDSJ` Term a -- Commutative Multiplicative Disjunction
 | Term a `NMCNJ` Term a -- Non-Commutative Associative Multiplicative Conjunction (like matrix multiplication for example)
 | Term a `NMDSJ` Term a -- Non-Commutative Associative Multiplicative Disjunction
 | Term a `CADSJ` Term a -- Commutative Additive Disjunction
 | Term a `CACNJ` Term a -- Commutative Additive Conjunction
 | LLINV (Term a) -- Linear Logic Multiplicative inverse, a^⊥
 | INVRT (Term a) -- Multiplicative inverse, a^-1 or like matrix inverse for non commutative
 | NEGAT (Term a) -- Additive negatation, negative numbers for example
 | OFCRS (Term a) -- Of-course exponential operator
 | WHYNT (Term a) -- Why-not exponential operator

 | Share UniqueShare
 | Focus (Term a)

 | Atom Descr

 | Expr a

 deriving (Show,Eq)

exprError = error "Expression must be converted to term"

-- predicate -> termFunctorM -> originalTerm -> m transformatedTerm
termMapM :: Monad m => (Term a -> Bool) -> (Term a -> m (Term a)) -> Term a -> m (Term a)
termMapM p f o@(a `CMCNJ` b) | p o = do c <- f a; d <- f b; return (c `CMCNJ` d)
termMapM p f o@(a `CMDSJ` b) | p o = do c <- f a; d <- f b; return (c `CMDSJ` d)
termMapM p f o@(a `NMCNJ` b) | p o = do c <- f a; d <- f b; return (c `NMCNJ` d)
termMapM p f o@(a `CADSJ` b) | p o = do c <- f a; d <- f b; return (c `CADSJ` d)
termMapM p f o@(a `CACNJ` b) | p o = do c <- f a; d <- f b; return (c `CACNJ` d)
termMapM p f o@(INVRT a) | p o = do b <- f a; return (INVRT b)
termMapM p f o@(NEGAT a) | p o = do b <- f a; return (NEGAT b)
termMapM p f o@(OFCRS a) | p o = do b <- f a; return (OFCRS b)
termMapM p f o@(WHYNT a) | p o = do b <- f a; return (WHYNT b)
termMapM p f o@(Focus a) | p o = do b <- f a; return (Focus b)
termMapM p _ o@(Expr  _) | p o = exprError
termMapM _ _ o = return o

toBinop (CMCNJ a b) = Just ((a,b),(CMCNJ,INVRT))
toBinop (CMDSJ a b) = Just ((a,b),(CMDSJ,INVRT))
toBinop (NMCNJ a b) = Just ((a,b),(NMCNJ,INVRT))
toBinop (CADSJ a b) = Just ((a,b),(CADSJ,NEGAT))
toBinop (CACNJ a b) = Just ((a,b),(CACNJ,NEGAT))
toBinop (Expr _) = exprError
toBinop _ = Nothing









