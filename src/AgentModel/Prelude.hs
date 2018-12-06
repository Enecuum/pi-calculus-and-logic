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
 [Left  $ agent { agentId =                   "Type/Integer"         ,  value = Symbol                        "Type", ports = ["Type"]                 }
 ,Left  $ agent { agentId =                   "Type/Channel"         ,  value = Symbol                        "Type", ports = ["Type"]                 }

 ,Left  $ agent {  agentId =           "Function/IntegerAdd"         ,  value = Symbol                    "Function"                                   }
 ,Right $ edge1 { fromPort = Port          "ClosureOperator" "Class" , toPort = Port                    "IntegerAdd" "Instance"                        }
 ,Right $ edge1 { fromPort = Port                  "Integer" "Type"  , toPort = Port                    "IntegerAdd" "Arg1/Type"                       }

 ,Left  $ agent {  agentId =           "Function/IntegerMul"         ,  value = Symbol                    "Function"                                   }
 ,Right $ edge1 { fromPort = Port          "ClosureOperator" "Class" , toPort = Port                    "IntegerAdd" "Instance"                        }
 ,Right $ edge1 { fromPort = Port                  "Integer" "Type"  , toPort = Port                    "IntegerAdd" "Arg1/Type"                       }

 ,Left  $ agent {  agentId = "Class/Function/BinaryOperator"         ,  value = Symbol                       "Class", ports = ["Arg1","Arg2","Result"] }
 ,Left  $ agent {  agentId =                      "B9wSQMV5"         ,  value = Symbol            "TypeIndet/ForAll"                                   }
 ,Left  $ agent {  agentId =                      "7BihLgw3"         ,  value = Symbol            "TypeIndet/ForAll"                                   }
 ,Left  $ agent {  agentId =                      "PeNcuHcB"         ,  value = Symbol            "TypeIndet/ForAll"                                   }
 ,Right $ edge1 { fromPort = Port                 "B9wSQMV5" "Type"  , toPort = Port "Class/Function/BinaryOperator" "Arg1/Type/In"                    }
 ,Right $ edge1 { fromPort = Port                 "7BihLgw3" "Type"  , toPort = Port "Class/Function/BinaryOperator" "Arg2/Type/In"                    }
 ,Right $ edge1 { fromPort = Port                 "PeNcuHcB" "Type"  , toPort = Port "Class/Function/BinaryOperator" "Result/Type/Out"                 }

 ,Left  $ agent {  agentId =         "Class/ClosureOperator"         ,  value = Symbol                       "Class"                                   }
 ,Right $ edge1 { fromPort = Port           "BinaryOperator" "Class" , toPort = Port               "ClosureOperator" "SuperClass"                      }
 ,Left  $ agent {  agentId =                      "dEZThAuo"         ,  value = Symbol            "TypeIndet/ForAll"                                   }
 ,Right $ edge1 { fromPort = Port                 "dEZThAuo" "Type"  , toPort = Port         "Class/ClosureOperator" "Arg1/Type"                       }
 ,Right $ edge1 { fromPort = Port                 "dEZThAuo" "Type"  , toPort = Port         "Class/ClosureOperator" "Arg2/Type"                       }
 ,Right $ edge1 { fromPort = Port                 "dEZThAuo" "Type"  , toPort = Port         "Class/ClosureOperator" "Result/Type"                     }



 ,Left  $ agent {  agentId =      "AssociativeOperator",          value = Symbol                     "Class"                                   }
 ,Right $ edge1 { fromPort = Port     "ClosureOperator" "Class", toPort = Port         "AssociativeOperator" "SuperClass"                      }
 ,Left  $ agent {  agentId =                 "s4CC2jGS",          value = Symbol "ClassIndeterminant/ForAll"                                   }
 ,Right $ edge1 { fromPort = Port "AssociativeOperator" "Class", toPort = Port                    "s4CC2jGS" "SuperClass"                      }
 ,Right $ edge1 { fromPort = Port            "phLkEMue" "Sort",  toPort = Port                    "s4CC2jGS" "Arg1/Sort"                       }
 ,Right $ edge1 { fromPort = Port            "QUzGSXMc" "Sort",  toPort = Port                    "s4CC2jGS" "Arg2/Sort"                       }
 ,Right $ edge1 { fromPort = Port            "8ZveTeZk" "Sort",  toPort = Port                    "s4CC2jGS" "Result/Sort"                     }
 ,Left  $ agent {  agentId =                 "PV3PA3Jg",          value = Symbol "ClassIndeterminant/ForAll"                                   }
 ,Right $ edge1 { fromPort = Port "AssociativeOperator" "Class", toPort = Port                    "PV4PA3Jg" "SuperClass"                      }
 ,Right $ edge1 { fromPort = Port            "8ZveTeZk" "Sort",  toPort = Port                    "PV4PA3Jg" "Arg1/Sort"                       }
 ,Right $ edge1 { fromPort = Port            "Px2WMRMG" "Sort",  toPort = Port                    "PV4PA3Jg" "Arg2/Sort"                       }
 ,Right $ edge1 { fromPort = Port            "kYcuEnPV" "Sort",  toPort = Port                    "PV4PA3Jg" "Result/Sort"                     }
 ,Left  $ agent {  agentId =                 "ZKmQEYbF",          value = Symbol "ClassIndeterminant/ForAll"                                   }
 ,Right $ edge1 { fromPort = Port "AssociativeOperator" "Class", toPort = Port                    "ZKmQEYbF" "SuperClass"                      }
 ,Right $ edge1 { fromPort = Port            "phLkEMue" "Sort",  toPort = Port                    "ZKmQEYbF" "Arg1/Sort"                       }
 ,Right $ edge1 { fromPort = Port            "X9Wffq8K" "Sort",  toPort = Port                    "ZKmQEYbF" "Arg2/Sort"                       }
 ,Right $ edge1 { fromPort = Port            "kYcuEnPV" "Sort",  toPort = Port                    "ZKmQEYbF" "Result/Sort"                     }
 ,Left  $ agent {  agentId =                 "Z9WH82Cy",          value = Symbol "ClassIndeterminant/ForAll"                                   }
 ,Right $ edge1 { fromPort = Port "AssociativeOperator" "Class", toPort = Port                    "Z9WH82Cy" "SuperClass"                      }
 ,Right $ edge1 { fromPort = Port            "QUzGSXMc" "Sort",  toPort = Port                    "Z9WH82Cy" "Arg1/Sort"                       }
 ,Right $ edge1 { fromPort = Port            "Px2WMRMG" "Sort",  toPort = Port                    "Z9WH82Cy" "Arg2/Sort"                       }
 ,Right $ edge1 { fromPort = Port            "X9Wffq8K" "Sort",  toPort = Port                    "Z9WH82Cy" "Result/Sort"                     }
 ,Left  $ agent {  agentId =                 "phLkEMue",          value = Symbol  "SortIndeterminant/ForAll"                                   }
 ,Left  $ agent {  agentId =                 "QUzGSXMc",          value = Symbol  "SortIndeterminant/ForAll"                                   }
 ,Left  $ agent {  agentId =                 "8ZveTeZk",          value = Symbol  "SortIndeterminant/ForAll"                                   }
 ,Left  $ agent {  agentId =                 "Px2WMRMG",          value = Symbol  "SortIndeterminant/ForAll"                                   }
 ,Left  $ agent {  agentId =                 "kYcuEnPV",          value = Symbol  "SortIndeterminant/ForAll"                                   }
 ,Left  $ agent {  agentId =                 "X9Wffq8K",          value = Symbol  "SortIndeterminant/ForAll"                                   }

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




