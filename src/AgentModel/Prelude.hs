{-# LANGUAGE FlexibleInstances, DuplicateRecordFields #-}

module AgentModel.Prelude where

import AgentModel.Types
import Data.Default.Class

prelude :: Net
prelude = Net agents edges
 where
  agents =
   [ dAgent { agentId = "Integer", ports = ["Type"], value = Symbol "IntegerType" }
   ]
  edges  =
   [
   ]

instance Default Net where
  def = prelude
  



