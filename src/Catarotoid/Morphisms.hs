module Catarotoid.Morphisms where

import Control.Concurrent.STM.TVar
import System.IO.Unsafe
import System.Random
import Data.Maybe
import Data.Functor.Identity
import Control.Monad.State.Lazy
import Data.Tuple

import Catarotoid.Term

-- predicate -> termAlgebraM -> original(Term z) -> m transformated(Term z)
termCataM :: Monad m => ((Term z) -> Bool) -> ((Term z) -> m (Term z)) -> (Term z) -> m (Term z)
termCataM p f a = termMapM p (termCataM p f) a >>= f

-- predicate -> termCoAlgebraM -> original(Term z) -> m transformated(Term z)
termAnaM :: Monad m => ((Term z) -> Bool) -> ((Term z) -> m (Term z)) -> (Term z) -> m (Term z)
termAnaM p f a = f a >>= termMapM p (termAnaM p f)

-- predicate -> termAlgebraM -> termCoAlgebraM -> original(Term z) -> m transformated(Term z)
termHyloM :: Monad m => ((Term z) -> Bool) -> ((Term z) -> m (Term z)) -> ((Term z) -> m (Term z)) -> (Term z) -> m (Term z)
termHyloM p f g a = g a >>= termMapM p (termHyloM p f g) >>= f

-- predicate -> termGAlgebraM -> original(Term z) -> m transformated(Term z)
termParaM :: Monad m => ((Term z) -> Bool) -> (((Term z), (Term z)) -> m (Term z)) -> (Term z) -> m (Term z)
termParaM p f a = termMapM p (termParaM p f) a >>= curry f a

-- predicate -> termGCoAlgebraM -> original(Term z) -> m tranformated(Term z)
termApoM :: Monad m => ((Term z) -> Bool) -> ((Term z) -> m (Either (Term z) (Term z))) -> (Term z) -> m (Term z)
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


