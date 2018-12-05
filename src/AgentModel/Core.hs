{-# LANGUAGE FlexibleInstances #-}

module AgentModel.Core where

import System.IO.Unsafe
import System.Random

data Polyedge = UniqueSingle | SharedUse deriving (Show)

data Value = Integer Integer | Symbol String | Path [Value] deriving (Show)
data Agent = Agent { agentId :: String, ports :: [String], value :: Value, typeOfAgent :: Maybe [Port] } deriving (Show)
data Port  = Port { idOfAgent :: String, port :: String, typeOfPort :: Maybe Port } deriving (Show,Eq)
data Edge  = Edge { polyedge :: Polyedge, directed :: Bool, fromPort :: Port, toPort :: Port, typeOfEdge :: Maybe Port } deriving (Show)
data Net   = Net { agents :: [Agent], edges :: [Edge] } deriving (Show)



partialEval :: (Port,Net) -> IO (Port,Net)
partialEval a = return a

followPort :: (Port,Net) -> Either Agent Edge
followPort (p@(Port a b q), Net c d) = case (f,e) of
   ([a],[]) -> Left  a
   ([],[a]) -> Right a
 where
  e = filter (\a -> fromPort a == p || toPort a == p) d 
  f = filter (\x -> agentId x == a && b `elem` ports x) c

setEdge :: Edge -> Net -> Net
setEdge o@(Edge q a b c d) (Net e f) = Net e (o:h)
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
  return $ dAgent { agentId = a, ports = ["Result"], value = Integer n, typeOfAgent = Just [dPort { idOfAgent = "IntegerOut", port = "Result" }] }

integerAdd :: Port -> Port -> Net -> IO (Port,Net)
integerAdd a b (Net agents edges) = do
  d <- randomStringId
  let e = dAgent { agentId = d, ports = ["Arg1","Arg2","Result"], value = Symbol "IntegerAdd" }
  let f = dEdge { directed = True, fromPort = a, toPort = dPort { idOfAgent = d, port = "Arg1" } }
  let g = dEdge { directed = True, fromPort = b, toPort = dPort { idOfAgent = d, port = "Arg2" } }
  let h = dPort { idOfAgent = d, port = "Result" }
  return (h,Net (e : agents) (f : g : edges))

integerMul :: Port -> Port -> Net -> IO (Port,Net)
integerMul a b (Net agents edges) = do
  d <- randomStringId
  let e = dAgent { agentId = d, ports = ["Arg1","Arg2","Result"], value = Symbol "IntegerMul" }
  let f = dEdge { directed = True, fromPort = a, toPort = dPort { idOfAgent = d, port = "Arg1" } }
  let g = dEdge { directed = True, fromPort = b, toPort = dPort { idOfAgent = d, port = "Arg2" } }
  let h = dPort { idOfAgent = d, port = "Result" }
  return (h,Net (e : agents) (f : g : edges))

piCalculusSend :: Port -> Port -> Port -> Net -> IO (Port,Net)
piCalculusSend a b c (Net agents edges) = do
  d <- randomStringId
  let e = dAgent { agentId = d, ports = ["ChanToPipe","NameToSend","SubseqProcess","Result"], value = Symbol "PiCalculusSend" }
  let f = dEdge { directed = True, fromPort = dPort { idOfAgent = d, port = "ChanToPipe" }, toPort = a }
  let g = dEdge { directed = True, fromPort = b, toPort = dPort { idOfAgent = d, port = "NameToSend" } }
  let h = dEdge { directed = True, fromPort = c, toPort = dPort { idOfAgent = d, port = "SubseqProcess" } }
  let i = dPort { idOfAgent = d, port = "Result" }
  return (i,Net (e : agents) (f : g : h : edges))

lambdaAbst :: Port -> Port -> Net -> IO (Port,Net)
lambdaAbst a b (Net agents edges) = do
  d <- randomStringId
  let e = dAgent { agentId = d, ports = ["Bind","Expr","Result"], value = Symbol "LambdaAbst" }
  let f = dEdge { directed = True, fromPort = dPort { idOfAgent = d, port = "Bind" }, toPort = a }
  let g = dEdge { directed = True, fromPort = b, toPort = dPort { idOfAgent = d, port = "Expr" } }
  let h = dPort { idOfAgent = d, port = "Result", typeOfPort = Nothing }
  return (h,Net (e : agents) (f : g : edges))

lambdaApply :: Port -> Port -> Net -> IO (Port,Net)
lambdaApply a b (Net agents edges) = do
  d <- randomStringId
  let e = dAgent { agentId = d, ports = ["Argument","Function","Result"], value = Symbol "LambdaApply" }
  let f = dEdge { directed = True, fromPort = a, toPort = dPort { idOfAgent = d, port = "Argument" } }
  let g = dEdge { directed = True, fromPort = b, toPort = dPort { idOfAgent = d, port = "Function" } }
  let h = dPort { idOfAgent = d, port = "Result" }
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

instance Num (Port,Net) where
  (a,Net b c) + (d,Net e f) = unsafePerformIO $ integerAdd a d (Net (b++e) (c++f))
  (a,Net b c) * (d,Net e f) = unsafePerformIO $ integerMul a d (Net (b++e) (c++f))
  fromInteger n = unsafePerformIO $ do
    o@(Agent a [b] c _) <- createIntegerAgent n
    return (Port a b Nothing, Net [o] [])



dPort  = Port undefined undefined Nothing
dEdge  = Edge UniqueSingle True undefined undefined Nothing
dAgent = Agent undefined [] (Symbol "DefaultAgent") Nothing







