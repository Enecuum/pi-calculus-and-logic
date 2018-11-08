{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, TypeFamilies, UndecidableInstances, ConstrainedClassMethods, AllowAmbiguousTypes, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, GADTs, IncoherentInstances, OverloadedLists, TemplateHaskell, DeriveLift #-}

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
import qualified GHC.Exts as E
--import Language.Haskell.TH
import Tools.FindCorrectTypesAtCompileTime
import Language.Haskell.TH.Syntax

-- type Vector = forall a . Vec 'EQ 0 a


class Snocable (a :: Ordering) (b :: Nat) where

instance Snocable 'EQ 0 where
instance Snocable 'LT 1 where

data Vec (a :: Ordering) (b :: Nat) c where
  Nil  :: Vec 'EQ 0 b
  Cons :: KnownNat b => c -> Vec (CmpNat (b-1) 0) (b-1) c -> Vec 'GT b c
  Snoc :: Snocable b c => Vec b c a -> a -> Vec 'LT 1 a

instance Lift (Vec 'EQ 0 a) where
  lift _ = return $ ConE $ mkName "Nil"

{-
instance {-# OVERLAPPABLE #-} (Snocable b c, Lift a) => Lift (Vec b c a) where
  lift o@Nil = lift (f o)
   where
    f :: Vec b c a -> Vec 'EQ 0 a
    f _ = Nil
  lift o@(Snoc _ _) = lift (f o)
   where
    f :: Vec b c a -> Vec 'LT 1 a
    f (Snoc a b) = Snoc a b
-}

instance (Lift a) => Lift (Vec 'LT 1 a) where
  lift (Snoc a b) = do
    c <- lift a
    d <- lift b
    return $ (VarE $ mkName "vecJoin") `AppE` c `AppE` ( (ConE $ mkName "Cons") `AppE` d `AppE` (ConE $ mkName "Nil") )

instance (Lift (Vec (CmpNat (n-1) 0) (n-1) a), Lift a) => Lift (Vec 'GT n a) where
  lift o@(Cons a b) = do
     c <- lift a
     d <- lift b
     return $ (ConE $ mkName "Cons") `AppE` c `AppE` d

infixr 8 `Cons`
infixl 8 `Snoc`

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

test0006 = [1,2,3,4,5] :: Vec 'GT 5 Double

instance E.IsList (Vec 'EQ 0 a) where
  type Item (Vec 'EQ 0 a) = a
  toList _ = []
  fromList [] = Nil
  fromListN 0 [] = Nil

instance E.IsList (Vec 'LT 1 a) where
  type Item (Vec 'LT 1 a) = a
  fromList [] = error "only non empty lists"
  fromList (x:xs) = foldl Snoc (Snoc Nil x) xs

instance ( KnownNat n, CmpNat n 0 ~ 'GT
         , E.Item (Vec (CmpNat (n - 1) 0) (n - 1) a) ~ a
         , E.IsList (Vec (CmpNat (n - 1) 0) (n - 1) a)
         , Foldable (Vec (CmpNat (n - 1) 0) (n - 1)) ) => E.IsList (Vec 'GT n a) where
  type Item (Vec 'GT n a) = a
  toList a = toList a
  fromList a = E.fromListN (length a) a
  fromListN n (x:xs) | n == v = a
                     | otherwise = error "invalid length"
    where
     a = Cons x (E.fromListN (n-1) xs)
     v :: Int
     v = fromInteger $ natVal $ f a
     f :: Vec 'GT n a -> Proxy n
     f = undefined
  

instance Functor (Vec 'EQ 0) where
  fmap f Nil = Nil

instance Functor (Vec (CmpNat (n-1) 0) (n-1)) => Functor (Vec 'GT n) where
  fmap f (Cons a b) = Cons (f a) (fmap f b)

useLiftWTH = "use lift function with template haskell"

instance Foldable (Vec 'EQ 0) where

  foldr f b Nil = b
  --foldr _ _ _ = error useLiftWTH

  foldl f b Nil = b
  --foldl _ _ _ = error useLiftWTH

instance Foldable (Vec (CmpNat (n-1) 0) (n-1)) => Foldable (Vec 'GT n) where
  foldr f b (Cons a c) = f a (foldr f b c)
  foldl f b (Cons a c) = foldl f (f b a) c

instance Zip (Vec 'EQ 0) where
  zip Nil Nil = Nil

instance Zip (Vec (CmpNat (n-1) 0) (n-1)) => Zip (Vec 'GT n) where
  zip (Cons a b) (Cons c d) = Cons (a,c) (zip b d)

instance Default (Vec 'EQ 0 a) where
  def = Nil

instance (KnownNat n, Default (Vec (CmpNat (n-1) 0) (n-1) a), Default a) => Default (Vec 'GT n a) where
  def = Cons def def

instance (Foldable (Vec a b), Show c) => Show (Vec a b c) where
  show a = show $ toList a

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

class VecJoin a b c d where
  vecJoin :: (CmpNat c 0 ~ 'GT) => Vec a b d -> Vec 'GT c d -> Vec 'GT (b+c) d

instance VecJoin 'EQ 0 c d where
  vecJoin _ a = a

instance (VecJoin (CmpNat (b-1) 0) (b-1) c d, CmpNat ((b + c) - 1) 0 ~ 'GT, ((b - 1) + c) ~ ((b + c) - 1), KnownNat (b+c)) => VecJoin 'GT b c d where
  vecJoin (Cons a b) c = Cons a $ vecJoin b c

class VecReverse a b c where
  vecReverse :: Vec a b c -> Vec a b c

instance VecReverse 'EQ 0 c where
  vecReverse _ = Nil

instance (((n - 1) + 1) ~ n, CmpNat n 0 ~ 'GT, VecReverse (CmpNat (n-1) 0) (n-1) c, VecJoin (CmpNat (n-1) 0) (n-1) 1 c) => VecReverse 'GT n c where
  vecReverse (Cons a b) = vecReverse b `vecJoin` Cons a Nil

class VecTranspose a b c d e where
  vecTranspose :: (KnownNat d, Functor (Vec a b)) => Vec a b (Vec c d e) -> Vec c d (Vec a b e)

instance VecTranspose 'GT b 'EQ 0 e where
  vecTranspose _ = Nil

instance (KnownNat (d-1), VecTranspose 'GT b (CmpNat (d-1) 0) (d-1) e) => VecTranspose 'GT b 'GT d e where
  vecTranspose a = Cons (fmap vecHead a) (vecTranspose $ fmap vecTail a)



