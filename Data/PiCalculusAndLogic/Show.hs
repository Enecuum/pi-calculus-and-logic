{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeSynonymInstances #-}

module Data.PiCalculusAndLogic.Show where

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
import Data.PiCalculusAndLogic.Tools

instance Show Term where
  show (Wait c 2) = show c ++ "()"
  show (Wait c x) = show c ++ "(" ++ show x ++ ")"
  show (Send c 1) = show c ++ "<>"
  show (Send c y) = show c ++ "<" ++ show y ++ ">"
  --show (Comm a) = foldr (\a b -> show a ++ "|" ++ b) (show $ head a) (tail a)
  --show (Ordr a) = foldl (\b a -> oqShow a ++ "." ++ b) (oqShow $ last a) (reverse $ init a)
  show (Serv a) = "!" ++ sqShow a
  show (OptF a) = "?" ++ sqShow a
  show (OptW c) = show c ++ "(?)"
  show (Log2 a) = "LN(" ++ show a ++ ")"
  show  Empt    = "0"
  show  Fail    = "1"

oqShow a@(Comm _) = "(" ++ show a ++ ")"
oqShow a = show a

sqShow a@(Comm _) = "(" ++ show a ++ ")"
sqShow a@(Ordr _) = "(" ++ show a ++ ")"
sqShow a = show a










