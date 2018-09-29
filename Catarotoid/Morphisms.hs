module Catarotoid.Morphisms where

import Control.Concurrent.STM.TVar
import System.IO.Unsafe
import System.Random
import Data.Maybe
import Data.Functor.Identity
import Control.Monad.State.Lazy
import Data.Tuple

import Catarotoid.Term

-- predicate -> termAlgebraM -> originalTerm -> m transformatedTerm
termCataM :: Monad m => (Term -> Bool) -> (Term -> m Term) -> Term -> m Term
termCataM p f a = termMapM p (termCataM p f) a >>= f

-- predicate -> termCoAlgebraM -> originalTerm -> m transformatedTerm
termAnaM :: Monad m => (Term -> Bool) -> (Term -> m Term) -> Term -> m Term
termAnaM p f a = f a >>= termMapM p (termAnaM p f)

-- predicate -> termAlgebraM -> termCoAlgebraM -> originalTerm -> m transformatedTerm
termHyloM :: Monad m => (Term -> Bool) -> (Term -> m Term) -> (Term -> m Term) -> Term -> m Term
termHyloM p f g a = g a >>= termMapM p (termHyloM p f g) >>= f

-- predicate -> termGAlgebraM -> originalTerm -> m transformatedTerm
termParaM :: Monad m => (Term -> Bool) -> ((Term, Term) -> m Term) -> Term -> m Term
termParaM p f a = termMapM p (termParaM p f) a >>= curry f a

termApoM :: Monad m => (Term -> Bool) -> (Term -> m (Either Term Term)) -> Term -> m Term
termApoM p f a = do
  b <- f a
  case b of
    Left  c -> return c
    Right d -> termMapM p (termApoM p f) d

termCata p f   a = runIdentity $ termCataM p (return . f) a
termAna  p f   a = runIdentity $ termAnaM  p (return . f) a
termHylo p f g a = runIdentity $ termHyloM p (return . f) (return . g) a
termPara p f   a = runIdentity $ termParaM p (return . f) a
termApo  p f   a = runIdentity $ termApoM  p (return . f) a


