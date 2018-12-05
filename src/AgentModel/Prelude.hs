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

preludeDecls :: [Either Agent Edge]
preludeDecls =
 [Left  $ Agent { agentId = "Integer", ports = ["Type"], value = Symbol "Type/Integer" }
 ,Left  $ Agent { agentId = "Channel", ports = ["Type"], value = Symbol "Type/PiCalculusChannel" }

 ,Left  $ Agent { agentId = "ClosureOperator", ports = ["Arg1","Arg2","Result"], value = Symbol "Class/ClosureOperator" }
 ,Left  $ Agent { agentId = "dEZThAuo", ports = ["Type"], value = Symbol "TypeIndeterminant" }
 ,Right $ Edge  { params  = Multi [Directed,SharedUse], fromPort = Port "dEZThAuo" "Type", toPort = Port "ClosureOperator" "Arg1/TypeIn" }
 ,Right $ Edge  { params  = Multi [Directed,SharedUse], fromPort = Port "dEZThAuo" "Type", toPort = Port "ClosureOperator" "Arg2/TypeIn" }
 ,Right $ Edge  { params  = Multi [Directed,SharedUse], fromPort = Port "dEZThAuo" "Type", toPort = Port "ClosureOperator" "Result/TypeOut" }

 ,Left  $ Agent { agentId = "CommutativeOperator", ports = ["Arg1","Arg2","Result"], value = Symbol "Class/CommutativeOperator" }
 ,Left  $ Agent { agentId = "2rBFz4sY", ports = ["Type"], value = Symbol "TypeIndeterminant" }
 ,Right $ Edge  { params  = Multi [Directed,SharedUse], fromPort = Port "2rBFz4sY" "Type", toPort = Port "CommutativeOperator" "Arg1/TypeIn" }
 ,Right $ Edge  { params  = Multi [Directed,SharedUse], fromPort = Port "2rBFz4sY" "Type", toPort = Port "CommutativeOperator" "Arg2/TypeIn" }
 ,Left  $ Agent { agentId = "6f58GwLo", ports = ["Arg1","Arg2","Result"], value = Symbol "ClassIndeterminant" }
 ,Right $ Edge  { params  = Multi [Directed,SharedUse], fromPort = Port "CommutativeOperator" "Class", toPort = Port "6f58GwLo" "SuperClass" }
 ,Right $ Edge  { params  = Multi [Directed,SharedUse], fromPort = Port "CbapTR4n" "Sort", toPort = Port "6f58GwLo" "Arg1/Sort/In" }
 ,Right $ Edge  { params  = Multi [Directed,SharedUse], fromPort = Port "1ivAVSN7" "Sort", toPort = Port "6f58GwLo" "Arg2/Sort/In" }
 ,Right $ Edge  { params  = Multi [Directed,SharedUse], fromPort = Port "mc56YAaK" "Sort", toPort = Port "6f58GwLo" "Result/Sort/Out" }
 ,Left  $ Agent { agentId = "JAVFH4ay", ports = ["Arg1","Arg2","Result"], value = Symbol "ClassIndeterminant" }
 ,Right $ Edge  { params  = Multi [Directed,SharedUse], fromPort = Port "CommutativeOperator" "Class", toPort = Port "JAVFH4ay" "SuperClass" }
 ,Right $ Edge  { params  = Multi [Directed,SharedUse], fromPort = Port "1ivAVSN7" "Sort", toPort = Port "JAVFH4ay" "Arg1/Sort/In" }
 ,Right $ Edge  { params  = Multi [Directed,SharedUse], fromPort = Port "CbapTR4n" "Sort", toPort = Port "JAVFH4ay" "Arg2/Sort/In" }
 ,Right $ Edge  { params  = Multi [Directed,SharedUse], fromPort = Port "mc56YAaK" "Sort", toPort = Port "JAVFH4ay" "Result/Sort/Out" }

 ]




