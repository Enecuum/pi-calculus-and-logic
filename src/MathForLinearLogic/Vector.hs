{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, TypeFamilies, UndecidableInstances, ConstrainedClassMethods, AllowAmbiguousTypes, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}

module MathForLinearLogic.Vector where

import System.Random
import Control.Monad
import Data.List
import Math.NumberTheory.Primes.Factorisation
import System.Random.Shuffle
import Math.NumberTheory.Primes.Testing
import GHC.TypeLits
import Data.Proxy
import Data.Functor


data VecNil a = VecNil

data a :. b = a :. b

infixr 8 :.

type family Vec (a :: Nat) b where
  Vec 0 a = ()
  Vec 1 a = a :. ()
  Vec n a = a :. Vec (n-1) a

class VecToList a b where
  vecToList :: a -> [b]

instance VecToList () b where
  vecToList () = []

instance VecToList b a => VecToList (a :. b) a where
  vecToList (a :. b) = a : vecToList b

instance (VecToList b a, VecToList (a :. b) a, Show a) => Show (a :. b) where
  show (a :. b) = show ( a : vecToList b )

vecSum a = sum $ vecToList a

instance Num () where
  () + () = ()
  () * () = ()

instance (Num a, Num b) => Num (a :. b) where
  (a :. b) + (c :. d) = (a+c) :. (b+d)
  (a :. b) * (c :. d) = (a*c) :. (b*d)


