{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeSynonymInstances #-}
module Data.SymbolForChan where

import Data.Word
import Data.List
import Data.String
import Data.Hashable
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import System.IO.Unsafe
import Data.IORef

test1212342 :: Chan
test1212342 = "vfsvfdsvsfdv"

{-# NOINLINE chanSymbolTable #-}
chanSymbolTable :: IORef (IntMap String)
chanSymbolTable = unsafePerformIO $ newIORef $ IntMap.empty

newtype Chan = Chan Int
 deriving (Eq,Ord)

instance Show Chan where
  show (Chan a) = unsafePerformIO $ do
    ioref <- readIORef chanSymbolTable
    case IntMap.lookup a ioref of
      Nothing -> return $ show a
      Just b  -> return b

instance Enum Chan where
  toEnum a = Chan $ toEnum a

instance Num Chan where
  fromInteger a = Chan $ fromInteger a

instance IsString Chan where
  fromString a = unsafePerformIO $ do
      ioref <- readIORef chanSymbolTable
      case IntMap.member d ioref of
        True  -> return (Chan d)
        False -> atomicModifyIORef' chanSymbolTable (\b -> (IntMap.insert d a b,Chan d))
    where
     b = hash a
     c = b `mod` 8999999999999
     d :: Int
     d = 1000000000000 + c

