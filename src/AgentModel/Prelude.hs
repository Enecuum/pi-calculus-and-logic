{-# LANGUAGE FlexibleInstances, DuplicateRecordFields #-}

module AgentModel.Prelude where

import AgentModel.Types
import Data.Default.Class
import Data.Either

instance Default Net where
  def = prelude

prelude :: Net
prelude = Net (map (fromLeft undefined) $ filter isLeft preludeDecls) (map (fromRight undefined) $ filter isRight preludeDecls)

-- graph directed edge is edge of flow of data from -> to, from is source of information of type or something else

edge1 = Edge (Multi [Directed,SharedUse]) undefined undefined
agent = Agent undefined [] undefined

preludeDecls :: [Either Agent Edge]
preludeDecls =
 [Left  $ agent { agentId = "Integer", ports = ["Type"], value = Symbol "Type/Integer" }
 ,Left  $ agent { agentId = "Channel", ports = ["Type"], value = Symbol "Type/PiCalculusChannel" }

 ,Left  $ agent {  agentId =           "BinaryOperator",          value = Symbol      "Class/BinaryOperator", ports = ["Arg1","Arg2","Result"] }
 ,Left  $ agent {  agentId =                 "B9wSQMV5",          value = Symbol  "TypeIndeterminant/ForAll"                            }
 ,Left  $ agent {  agentId =                 "7BihLgw3",          value = Symbol  "TypeIndeterminant/ForAll"                                   }
 ,Left  $ agent {  agentId =                 "PeNcuHcB",          value = Symbol  "TypeIndeterminant/ForAll"                                   }
 ,Right $ edge1 { fromPort = Port            "B9wSQMV5" "Type",  toPort = Port              "BinaryOperator" "Arg1/Type/In"                    }
 ,Right $ edge1 { fromPort = Port            "7BihLgw3" "Type",  toPort = Port              "BinaryOperator" "Arg2/Type/In"                    }
 ,Right $ edge1 { fromPort = Port            "PeNcuHcB" "Type",  toPort = Port              "BinaryOperator" "Result/Type/Out"                 }

 ,Left  $ agent {  agentId =          "ClosureOperator",          value = Symbol     "Class/ClosureOperator"                                   }
 ,Right $ edge1 { fromPort = Port      "BinaryOperator" "Class", toPort = Port             "ClosureOperator" "SuperClass"                      }
 ,Left  $ agent {  agentId =                 "dEZThAuo",          value = Symbol  "TypeIndeterminant/ForAll"                                   }
 ,Right $ edge1 { fromPort = Port            "dEZThAuo" "Type",  toPort = Port             "ClosureOperator" "Arg1/Type/In"                    }
 ,Right $ edge1 { fromPort = Port            "dEZThAuo" "Type",  toPort = Port             "ClosureOperator" "Arg2/Type/In"                    }
 ,Right $ edge1 { fromPort = Port            "dEZThAuo" "Type",  toPort = Port             "ClosureOperator" "Result/Type/Out"                 }

 ,Left  $ agent {  agentId =      "CommutativeOperator",          value = Symbol "Class/CommutativeOperator"                                   }
 ,Right $ edge1 { fromPort = Port      "BinaryOperator" "Class", toPort = Port         "CommutativeOperator" "SuperClass"                      }
 ,Left  $ agent {  agentId =                 "2rBFz4sY",          value = Symbol  "TypeIndeterminant/ForAll"                                   }
 ,Right $ edge1 { fromPort = Port            "2rBFz4sY" "Type",  toPort = Port         "CommutativeOperator" "Arg1/Type/In"                    }
 ,Right $ edge1 { fromPort = Port            "2rBFz4sY" "Type",  toPort = Port         "CommutativeOperator" "Arg2/Type/In"                    }
 ,Left  $ agent {  agentId =                 "6f58GwLo",          value = Symbol "ClassIndeterminant/ForAll"                                   }
 ,Right $ edge1 { fromPort = Port "CommutativeOperator" "Class", toPort = Port                    "6f58GwLo" "SuperClass"                      }
 ,Right $ edge1 { fromPort = Port            "CbapTR4n" "Sort",  toPort = Port                    "6f58GwLo" "Arg1/Sort/In"                    }
 ,Right $ edge1 { fromPort = Port            "1ivAVSN7" "Sort",  toPort = Port                    "6f58GwLo" "Arg2/Sort/In"                    }
 ,Right $ edge1 { fromPort = Port            "mc56YAaK" "Sort",  toPort = Port                    "6f58GwLo" "Result/Sort/Out"                 }
 ,Left  $ agent {  agentId =                 "JAVFH4ay",          value = Symbol "ClassIndeterminant/ForAll"                                   }
 ,Right $ edge1 { fromPort = Port "CommutativeOperator" "Class", toPort = Port                    "JAVFH4ay" "SuperClass"                      }
 ,Right $ edge1 { fromPort = Port            "1ivAVSN7" "Sort",  toPort = Port                    "JAVFH4ay" "Arg1/Sort/In"                    }
 ,Right $ edge1 { fromPort = Port            "CbapTR4n" "Sort",  toPort = Port                    "JAVFH4ay" "Arg2/Sort/In"                    }
 ,Right $ edge1 { fromPort = Port            "mc56YAaK" "Sort",  toPort = Port                    "JAVFH4ay" "Result/Sort/Out"                 }
 ,Left  $ agent {  agentId =                 "1ivAVSN7",          value = Symbol  "SortIndeterminant/ForAll"                                   }
 ,Left  $ agent {  agentId =                 "CbapTR4n",          value = Symbol  "SortIndeterminant/ForAll"                                   }
 ,Left  $ agent {  agentId =                 "mc56YAaK",          value = Symbol  "SortIndeterminant/ForAll"                                   }

 ]




