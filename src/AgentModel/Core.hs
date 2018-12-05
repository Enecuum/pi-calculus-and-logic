{-# LANGUAGE FlexibleInstances, DuplicateRecordFields #-}

module AgentModel.Core where

import System.IO.Unsafe
import System.Random
import Data.Default.Class

instance Show EdgeParams where
  show Directed = "D"
  show UniqueSingle = "U"
  show SharedUse = "S"
  show (Multi a) = concatMap show a

data EdgeParams = Directed | UniqueSingle | SharedUse | Multi [EdgeParams]
data Value      = Integer Integer | Symbol String     | Path  [Value]          deriving (Show)

data Edge  = Edge  { params  :: EdgeParams, fromPort :: Port, toPort :: Port } deriving (Show)
data Agent = Agent { agentId :: String,  ports :: [String], value :: Value }   deriving (Show)
data Port  = Port  { agentId :: String,  port  ::  String }                    deriving (Eq)
data Net   = Net   { agents  :: [Agent], edges :: [Edge]  }                    deriving (Show)


partialEval :: (Port,Net) -> IO (Port,Net)
partialEval a = return a

followPort :: (Port,Net) -> Either Agent Edge
followPort (p@(Port a b), Net c d) = case (f,e) of
   ([a],[]) -> Left  a
   ([],[a]) -> Right a
 where
  e = filter (\a -> fromPort a == p || toPort a == p) d 
  f = filter (\(Agent id po va) -> id == a && b `elem` po) c

setEdge :: Edge -> Net -> Net
setEdge o@(Edge a b c) (Net e f) = Net e (o:h)
 where
  pred x = fromPort x == b && toPort x == c
  h = filter (not.pred) f


class TypeInference a where
  typeInference :: (a,Net) -> IO (a,Net)  

instance TypeInference Port where
  typeInference a@(b,c) = case followPort a of
    Left  d -> do e <- typeInference (d,c); return $ (b,snd e)
    Right d -> do e <- typeInference (d,c); return $ (b,snd e)

instance TypeInference Agent where
  typeInference a@(b,c) = undefined

instance TypeInference Edge where
  typeInference a@(b,c) = undefined


createIntegerAgent :: Integer -> IO Agent
createIntegerAgent n = do
  a <- randomStringId
  return $ dAgent { agentId = a, ports = ["Result"], value = Integer n }

integerAdd :: Port -> Port -> Net -> IO (Port,Net)
integerAdd a b (Net agents edges) = do
  d <- randomStringId
  let e = dAgent { agentId = d, ports = ["Arg1","Arg2","Result"], value = Symbol "IntegerAdd" }
  let f = dEdge { fromPort = a, toPort = dPort { agentId = d, port = "Arg1" } }
  let g = dEdge { fromPort = b, toPort = dPort { agentId = d, port = "Arg2" } }
  let h = dPort { agentId = d, port = "Result" }
  return (h,Net (e : agents) (f : g : edges))

integerMul :: Port -> Port -> Net -> IO (Port,Net)
integerMul a b (Net agents edges) = do
  d <- randomStringId
  let e = dAgent { agentId = d, ports = ["Arg1","Arg2","Result"], value = Symbol "IntegerMul" }
  let f = dEdge { fromPort = a, toPort = dPort { agentId = d, port = "Arg1" } }
  let g = dEdge { fromPort = b, toPort = dPort { agentId = d, port = "Arg2" } }
  let h = dPort { agentId = d, port = "Result" }
  return (h,Net (e : agents) (f : g : edges))

piCalculusSend :: Port -> Port -> Port -> Net -> IO (Port,Net)
piCalculusSend a b c (Net agents edges) = do
  d <- randomStringId
  let e = dAgent { agentId = d, ports = ["ChanToPipe","NameToSend","SubseqProcess","Result"], value = Symbol "PiCalculusSend" }
  let f = dEdge { fromPort = dPort { agentId = d, port = "ChanToPipe" }, toPort = a }
  let g = dEdge { fromPort = b, toPort = dPort { agentId = d, port = "NameToSend" } }
  let h = dEdge { fromPort = c, toPort = dPort { agentId = d, port = "SubseqProcess" } }
  let i = dPort { agentId = d, port = "Result" }
  return (i,Net (e : agents) (f : g : h : edges))

lambdaAbst :: Port -> Port -> Net -> IO (Port,Net)
lambdaAbst a b (Net agents edges) = do
  d <- randomStringId
  let e = dAgent { agentId = d, ports = ["Bind","Expr","Result"], value = Symbol "LambdaAbst" }
  let f = dEdge { fromPort = dPort { agentId = d, port = "Bind" }, toPort = a }
  let g = dEdge { fromPort = b, toPort = dPort { agentId = d, port = "Expr" } }
  let h = dPort { agentId = d, port = "Result" }
  return (h,Net (e : agents) (f : g : edges))

lambdaApply :: Port -> Port -> Net -> IO (Port,Net)
lambdaApply a b (Net agents edges) = do
  d <- randomStringId
  let e = dAgent { agentId = d, ports = ["Argument","Function","Result"], value = Symbol "LambdaApply" }
  let f = dEdge { fromPort = a, toPort = dPort { agentId = d, port = "Argument" } }
  let g = dEdge { fromPort = b, toPort = dPort { agentId = d, port = "Function" } }
  let h = dPort { agentId = d, port = "Result" }
  return (h,Net (e : agents) (f : g : edges))


randomStringId = do
  a <- randomRIO (2^46,2^48 :: Integer)
  return $ take 8 $ f a
 where
  list = "123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ"
  f 0 = []
  f n = ( list !! (fromInteger $ n `mod` 58) ) : f (n `div` 58)

n :: Integer -> (Port,Net)
n = fromInteger

instance {-# OVERLAPPING #-} Show (Port,Net) where
  show (p,n) = "========\n" ++ show p ++ "\n" ++ show n ++ "\n========\n"

instance {-# OVERLAPPING #-} Show [Agent] where
  show a = unlines $ "" : map (("    "++).show) a

instance {-# OVERLAPPING #-} Show [Edge] where
  show a = unlines $ "" : map (("    "++).show) a

instance Show Port where
  show (Port a b) = a ++ ":" ++ b

instance Num (Port,Net) where
  (a,Net b c) + (d,Net e f) = unsafePerformIO $ integerAdd a d (Net (b++e) (c++f))
  (a,Net b c) * (d,Net e f) = unsafePerformIO $ integerMul a d (Net (b++e) (c++f))
  fromInteger n = unsafePerformIO $ do
    o@(Agent a [b] c) <- createIntegerAgent n
    let g = dEdge { params = Multi [Directed,SharedUse], fromPort = Port "Integer" "Type", toPort = Port a (b ++ "TypeIn") }
    return (Port a b, Net [o] [g])




dPort  = Port undefined undefined
dEdge  = Edge (Multi [Directed,UniqueSingle]) undefined undefined
dAgent = Agent undefined [] (Symbol "DefaultAgent")






