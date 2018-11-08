{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, TypeFamilies, UndecidableInstances, ConstrainedClassMethods, AllowAmbiguousTypes, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, GADTs, IncoherentInstances, OverloadedLists, TemplateHaskell #-}

module MathForLinearLogic.Test01 where

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
import MathForLinearLogic.Vector

test0008 = $(lift ( [1,2,3,4,5] :: Vec 'GT $(detectNat) Double ) )

test0009 = $(lift ( [1,2,3,4,5] :: Vec 'GT $(detectNat) Double ) )

--test0007 = [1,2,3,4,5] :: Vec 'GT $(detectNat) Double

--test0008 = $(lift ( [1,2,3,4,5] :: Vec 'GT $(detectNat) Double ) )

--test0008 = $(lift ( [1,2,3,4,5] :: Vec 'GT $(detectNat) Double ) )

--test0008 = $(liftString "( [1,2,3,4,5] :: Vec 'GT 5 Double )" )

-- $(findCorrectTypesAtCompileTime [ [1,2,3,4,5] :: (KnownNat n, n ~ 5) => Vec 'GT n Double ] )



