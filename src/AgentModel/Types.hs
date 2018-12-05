{-# LANGUAGE FlexibleInstances, DuplicateRecordFields, FlexibleContexts #-}

module AgentModel.Types where

import System.IO.Unsafe
import System.Random
import Data.Default.Class
import Data.List

instance Show EdgeParams where
  show Directed     = "D"
  show UniqueSingle = "U"
  show SharedUse    = "S"
  show (Multi a) = concatMap show a

data EdgeParams = Directed | UniqueSingle | SharedUse | Multi [EdgeParams]     deriving (Eq)
data Value      = Integer Integer | Symbol String     | Path  [Value]          deriving (Show,Eq)

data Edge  = Edge  { params  :: EdgeParams, fromPort :: Port, toPort :: Port } deriving (Show,Eq)
data Agent = Agent { agentId :: String,  ports :: [String], value :: Value }   deriving (Show,Eq)
data Port  = Port  { agentId :: String,  port  ::  String }                    deriving (Eq)
data Net   = Net   { agents  :: [Agent], edges :: [Edge]  }                    deriving (Show)

dPort  = Port undefined undefined
dEdge  = Edge (Multi [Directed,UniqueSingle]) undefined undefined
dAgent = Agent undefined [] (Symbol "DefaultAgent")

instance {-# OVERLAPPING #-} Show (Port,Net) where
  show (p,n) = "========\n" ++ show p ++ "\n" ++ show n ++ "\n========\n"

instance {-# OVERLAPPING #-} Show [Agent] where
  show a = unlines $ "" : map (("    "++).show) a

instance {-# OVERLAPPING #-} Show [Edge] where
  show a = unlines $ "" : map (("    "++).show) a

instance Show Port where
  show (Port a b) = a ++ ":" ++ b






