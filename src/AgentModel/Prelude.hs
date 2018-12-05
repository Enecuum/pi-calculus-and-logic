
module AgentModel.Prelude where

import AgentModel.Core

prelude :: Net
prelude = Net agents edges
 where
  agents =
   [ dAgent { agentId = "Integer", ports = ["Type"], value = Symbol "IntegerType" }
   , dAgent { agentId = "IntegerOut", ports = ["Arg1","Result"],  value = Symbol "OutputTypeFunction" }
   , dAgent { agentId = "IntegerIn",  ports = ["Arg1","Require"], value = Symbol "InputTypeFunction"  }
   ]
  edges  =
   [ dEdge { polyedge = SharedUse, directed = True
           , fromPort = dPort { idOfAgent = "Integer",    port = "Type" }
           , toPort   = dPort { idOfAgent = "IntegerOut", port = "Arg1" } }
   , dEdge { polyedge = SharedUse, directed = True
           , fromPort = dPort { idOfAgent = "IntegerIn",  port = "Arg1" }
           , toPort   = dPort { idOfAgent = "Integer",    port = "Type" } }
   ]

