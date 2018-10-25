{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeSynonymInstances #-}

module Data.PiCalculusAndLogic.Tools where

import Data.Word
import Data.List
import Data.String
import Data.Hashable
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import System.IO.Unsafe
import Data.IORef
import Data.SymbolForChan
import Data.PiCalculusAndLogic.Term
import Data.PiCalculusAndLogic.Expr

(Ordr a) <> (Ordr b) = Ordr (a++b)
(Ordr a) <>       b  = Ordr (a++[b])
(     a) <> (Ordr b) = Ordr (a:b)
(     a) <>       b  = Ordr [a,b]

(Comm a) *| (Comm b) = Comm (sort (a++b))
(Comm a) *|       b  = Comm (sort (a++[b]))
(     a) *| (Comm b) = Comm (sort (a:b))
(     a) *|       b  = Comm (sort [a,b])

infixr 7 *|
infixr 8 <>

