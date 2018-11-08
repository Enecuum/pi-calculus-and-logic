{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, TypeFamilies, UndecidableInstances, ConstrainedClassMethods, AllowAmbiguousTypes, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, GADTs, IncoherentInstances #-}

module MathForLinearLogic.Vector where

import Prelude hiding (zip)
import System.Random
import Control.Monad
import Data.List hiding (zip)
import Math.NumberTheory.Primes.Factorisation
import System.Random.Shuffle
import Math.NumberTheory.Primes.Testing
import GHC.TypeLits
import Data.Proxy
import Data.Functor
import Data.Foldable
import Data.Zip
import Data.Default.Class

infixr 8 `Cons`

data Vec (a :: Ordering) (b :: Nat) c where
  Nil  :: Vec 'EQ 0 b
  Cons :: KnownNat b => c -> Vec (CmpNat (b-1) 0) (b-1) c -> Vec 'GT b c

instance Functor (Vec 'EQ 0) where
  fmap f Nil = Nil

instance Functor (Vec (CmpNat (n-1) 0) (n-1)) => Functor (Vec 'GT n) where
  fmap f (Cons a b) = Cons (f a) (fmap f b)

instance Foldable (Vec 'EQ 0) where
  foldr f b Nil = b

instance Foldable (Vec (CmpNat (n-1) 0) (n-1)) => Foldable (Vec 'GT n) where
  foldr f b (Cons a c) = f a (foldr f b c)

instance Zip (Vec 'EQ 0) where
  zip Nil Nil = Nil

instance Zip (Vec (CmpNat (n-1) 0) (n-1)) => Zip (Vec 'GT n) where
  zip (Cons a b) (Cons c d) = Cons (a,c) (zip b d)

instance Default (Vec 'EQ 0 a) where
  def = Nil

instance (KnownNat n, Default (Vec (CmpNat (n-1) 0) (n-1) a), Default a) => Default (Vec 'GT n a) where
  def = Cons def def

test0001 :: Vec 'GT 4 Int
test0001 = 1 `Cons` 2 `Cons` 3 `Cons` 4 `Cons` Nil

test0002 :: Vec 'GT 4 ()
test0002 = def

test0003 :: Vec 'GT 7 Int
test0003 = fromInteger 3

instance (Foldable (Vec a b), Show c) => Show (Vec a b c) where
  show a = show $ foldr (:) [] a

instance (Functor (Vec a b), Zip (Vec a b), Default (Vec a b ()), Num c) => Num (Vec a b c) where
  a + b = fmap (uncurry (+)) $ zip a b
  a * b = fmap (uncurry (*)) $ zip a b
  abs a = fmap abs a
  signum a = fmap signum a
  negate a = fmap negate a
  fromInteger n = a
   where
    f :: (Functor (Vec a b), Zip (Vec a b), Default (Vec a b ()), Num c) => Vec a b c -> Vec a b ()
    f _ = def
    a = fmap (const $ fromInteger n) $ f a




