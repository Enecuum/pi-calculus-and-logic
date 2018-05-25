{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeSynonymInstances #-}

module Data.PiCalculusAndLogic.Term where

import Data.SymbolForChan

data Term = Empt | Fail
          | Send { chanToPipe :: Chan, nameToSend :: Chan }
          | Wait { chanToWait :: Chan, nameToBind :: Chan }
          | OptW { chanToWait :: Chan }
          | OptF  Term
          | Comm [Term]
          | Ordr [Term]
          | Serv  Term
          | Log2  Term

 deriving (Eq,Ord)










