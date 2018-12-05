{-# LANGUAGE FlexibleInstances, DuplicateRecordFields #-}

module AgentModel.Prelude where

import AgentModel.Core
import Data.Default.Class

prelude :: Net
prelude = Net agents edges
 where
  agents =
   [ dAgent { agentId = "Integer", ports = ["Type"], value = Symbol "IntegerType" }
   , dAgent { agentId = "IntegerOut", ports = ["Arg1","Result"],  value = Symbol "OutputTypeFunction" }
   , dAgent { agentId = "IntegerIn",  ports = ["Arg1","Require"], value = Symbol "InputTypeFunction"  }
   ]
  edges  =
   [ dEdge { params = Multi [Directed,SharedUse]
           , fromPort = dPort { agentId = "Integer",    port = "Type" }
           , toPort   = dPort { agentId = "IntegerOut", port = "Arg1" } }
   , dEdge { params = Multi [Directed,SharedUse]
           , fromPort = dPort { agentId = "IntegerIn",  port = "Arg1" }
           , toPort   = dPort { agentId = "Integer",    port = "Type" } }
   ]

instance Default Net where
  def = prelude
  



