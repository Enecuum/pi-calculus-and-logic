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

test0001 :: Vec 'GT 4 Int
test0001 = 1 `Cons` 2 `Cons` 3 `Cons` 4 `Cons` Nil

test0002 :: Vec 'GT 4 ()
test0002 = def

test0003 :: Vec 'GT 7 Int
test0003 = fromInteger 3

test0004 :: Vec 'GT 4 (Vec 'GT 3 Int)
test0004 = (1 `Cons` 2 `Cons` 3 `Cons` Nil)
    `Cons` (4 `Cons` 5 `Cons` 6 `Cons` Nil)
    `Cons` (7 `Cons` 8 `Cons` 9 `Cons` Nil)
    `Cons` (3 `Cons` 3 `Cons` 3 `Cons` Nil)
    `Cons` Nil

test0005 = vecTranspose test0004

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

vecHead :: Vec 'GT b c -> c
vecHead (Cons a _) = a

vecTail :: Vec 'GT n c -> Vec (CmpNat (n-1) 0) (n-1) c
vecTail (Cons _ a) = a

class VecTranspose a b c d e where
  vecTranspose :: (KnownNat d, Functor (Vec a b)) => Vec a b (Vec c d e) -> Vec c d (Vec a b e)

instance VecTranspose 'GT b 'EQ 0 e where
  vecTranspose _ = Nil

instance (KnownNat b, KnownNat (d-1), VecTranspose 'GT b (CmpNat (d-1) 0) (d-1) e) => VecTranspose 'GT b 'GT d e where
  vecTranspose a = Cons (fmap vecHead a) (vecTranspose $ fmap vecTail a)





