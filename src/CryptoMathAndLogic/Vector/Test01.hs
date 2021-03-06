{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, TypeFamilies, UndecidableInstances, ConstrainedClassMethods, AllowAmbiguousTypes, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, GADTs, IncoherentInstances, OverloadedLists, TemplateHaskell, RankNTypes #-}

module CryptoMathAndLogic.Vector.Test01 where

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
--import Tools.FindCorrectTypesAtCompileTime
import Language.Haskell.TH.Syntax
import CryptoMathAndLogic.Vector
import Data.Dynamic

test0008 = $(lift ( [1,2,3,4,5] :: Vec 'LT 1 Double ) )

test0009 = $(lift ( [1,2,3,4,5,6] :: Vec 'LT 1 Int ) )

test0010 = $(lift ( [1,2,3,4,5,6,7 :: Integer] :: Vec 'LT 1 Integer ) )

test0011 = $(lift ( [] :: Vec 'EQ 0 Integer ) )

test0012 = $(lift ( [[1,2],[3,4],[5,6]] :: Vec 'LT 1 (Vec 'LT 1 Int) ) )

test0013 = $(lift ( [1,2,3,4,5,6,7 :: Float] :: Vec 'LT 1 Float ) )



