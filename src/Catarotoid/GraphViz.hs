module Catarotoid.GraphViz where

import System.Random
import System.IO.Unsafe
import Data.Maybe
import Debug.Trace
import Data.Functor.Identity
import Control.Monad.State.Lazy
import Data.List
import Data.IORef

import Catarotoid.Term
import Catarotoid.UniqueShare

termToGraphViz a = do writeFile "/tmp/1.dot" "\n"
                      putStr "digraph {\n"
                      putStr $ "  graph [pad=\"0.5\", ranksep=\"0.525\", nodesep=\"2\"]\n"
                      --putStr "SharNELL [shape=folder]\n"
                      --fun "SharNELL" a
                      cc <- randomRIO (2^32,2^48 :: Int)
                      c <- mapM (\(a,b) -> do c <- randomRIO (2^32,2^48 :: Int)
                                              let d = "rotation" ++ show c
                                              putStr $ " subgraph cluster_5" ++ show c ++ " {\n"
                                              putStr " label = \"Rotation\"\n"
                                              putStr " fontsize = 28\n"
                                              putStr $ " subgraph cluster_3" ++ show c ++ " {\n"
                                              putStr " fontsize = 24\n"
                                              putStr " label = \"⧖ Focused\"\n"
                                              putStr $ "  " ++ d ++ " [label=\"<L>|L ⊗ R = 1|<R>\",shape=Mrecord,fontsize=35,pos=\"0,0!\"]\n"
                                              putStr $ " }\n"
                                              let e = do putStr $ " subgraph cluster_1" ++ show c ++ " {\n"
                                                         putStr " label = \"Term\"\n"
                                                         putStr " fontsize = 20\n"
                                                         fun (d ++ ":L") a
                                                         putStr $ " }\n"
                                                         putStr $ " subgraph cluster_2" ++ show c ++ " {\n"
                                                         putStr " label = \"Term\"\n"
                                                         putStr " fontsize = 20\n"
                                                         fun (d ++ ":R") b
                                                         putStr $ " }\n"
                                              e
                                              putStr $ " }\n"
                                              return e
                                      ) a
                      --mapM_ id c
                      shrs <- readIORef shares
                      --putStr $ " subgraph cluster_8" ++ show cc ++ " {\n"
                      --putStr $ " height=9"
                      mapM_ (\(a,b) -> do c <- randomRIO (2^32,2^48 :: Int)
                                          putStr $ "  " ++ b ++ " [shape=folder,fontsize=30]"
                                          putStr $ "  " ++ a ++ " -> " ++ b ++ "[ minlen=10 ]\n"
                               ) shrs
                      --putStr $ " }\n"
                      putStr "}"
 where
   putStr = appendFile "/tmp/1.dot"
   shares = unsafePerformIO $ newIORef []

   fun link MROOT = do
    c <- randomRIO (2^32,2^48 :: Int)
    let d = "Root"
    putStr $ "  " ++ d ++ " [label=\"SharNELL\",shape=folder]\n"
    putStr $ "  " ++ link ++ " -> " ++ d ++ "\n"

   fun link (a `CMCNJ` b) = do
    c <- randomRIO (2^32,2^48 :: Int)
    let d = "times" ++ show c
    putStr $ "  " ++ d ++ " [label=\"⊗\",shape=none,fontsize=40]\n"
    putStr $ "  " ++ link ++ " -> " ++ d ++ "\n"
    fun d a
    fun d b

   fun link (a `CMDSJ` b) = do
    c <- randomRIO (2^32,2^48 :: Int)
    let d = "mdsj" ++ show c
    putStr $ "  " ++ d ++ " [label=\"⅋\",shape=none,fontsize=40]\n"
    putStr $ "  " ++ link ++ " -> " ++ d ++ "\n"
    fun d a
    fun d b

   fun link (a `NMCNJ` b) = do
    c <- randomRIO (2^32,2^48 :: Int)
    let d = "concat" ++ show c
    putStr $ "  " ++ d ++ " [label=\"<L>L|<R>R\",shape=Mrecord]\n"
    putStr $ "  " ++ link ++ " -> " ++ d ++ "\n"
    fun (d ++ ":L") a
    fun (d ++ ":R") b

{-
   fun link (Redu a) = do
    c <- randomRIO (2^32,2^48 :: Int)
    let d = "redu" ++ show c
    putStr $ "  " ++ d ++ " [label=\"Reduced\",shape=folder,fontsize=25]\n"
    putStr $ "  " ++ link ++ " -> " ++ d ++ "\n"
    fun d a

   fun link Unit = do
    c <- randomRIO (2^32,2^48 :: Int)
    let d = "unit" ++ show c
    putStr $ "  " ++ d ++ " [label=\"Unit\"]\n"
    putStr $ "  " ++ link ++ " -> " ++ d ++ "\n"
   fun link (Favo (Inve (Atom a))) = do
    c <- randomRIO (2^32,2^48 :: Int)
    let d = "Atom" ++ show c
    putStr $ "  " ++ d ++ " [label=\"⬔\n⬒\n{" ++ take 3 (show a) ++ "}\",shape=none,fontsize=40]\n"
    putStr $ "  " ++ link ++ " -> " ++ d ++ "\n"
   fun link (Inve (Favo (Atom a))) = do
    c <- randomRIO (2^32,2^48 :: Int)
    let d = "Atom" ++ show c
    putStr $ "  " ++ d ++ " [label=\"⬒\n⬔\n{" ++ take 3 (show a) ++ "}\",shape=none,fontsize=40]\n"
    putStr $ "  " ++ link ++ " -> " ++ d ++ "\n"

   fun link (Favo (Atom a)) = do
    c <- randomRIO (2^32,2^48 :: Int)
    let d = "Atom" ++ show c
    putStr $ "  " ++ d ++ " [label=\"⬔\n{" ++ take 3 (show a) ++ "}\",shape=none,fontsize=40]\n"
    putStr $ "  " ++ link ++ " -> " ++ d ++ "\n"
-}


   fun link (INVRT (Atom a)) = do
    c <- randomRIO (2^32,2^48 :: Int)
    let d = "Atom" ++ show c
    putStr $ "  " ++ d ++ "[label=\"⬔\n{" ++ tail (show a) ++ "}\",shape=none,fontsize=40]\n"
    putStr $ "  " ++ link ++ " -> " ++ d ++ "\n"

   fun link (INVRT a) = do
    c <- randomRIO (2^32,2^48 :: Int)
    let d = "Favo" ++ show c
    putStr $ "  " ++ d ++ " [label=\"⬔\",shape=none,fontsize=40]\n"
    putStr $ "  " ++ link ++ " -> " ++ d ++ "\n"
    fun d a

   fun link (Focus a) = do
    c <- randomRIO (2^32,2^48 :: Int)
    let d = "Focu" ++ show c
    putStr $ "  " ++ d ++ " [label=\"⧖\",shape=none,fontsize=40]\n"
    putStr $ "  " ++ link ++ " -> " ++ d ++ "\n"
    fun d a

   fun link (Share (USH a)) = do
    let d = "Id" ++ take 3 (show a)
    c <- randomRIO (2^32,2^48 :: Int)
    let e = "shar" ++ show c
    putStr $ "  " ++ e ++ " [label=\"Shared" ++ take 0 (show a) ++ "\",shape=octagon,fontsize=15]\n"
    --let shar = "  " ++ d ++ " [label=\"Shared" ++ take 0 (show a) ++ "\",shape=octagon,fontsize=25]\n"
    modifyIORef shares (\a -> (e,d) : a)
    putStr $ "  " ++ link ++ " -> " ++ e ++ "\n"

   fun link (Atom a) = do
    c <- randomRIO (2^32,2^48 :: Int)
    let d = "Atom" ++ show c
    putStr $ "  " ++ d ++ " [label=\"{" ++ tail (show a) ++ "}\",shape=none,fontsize=40]\n"
    putStr $ "  " ++ link ++ " -> " ++ d ++ "\n"

