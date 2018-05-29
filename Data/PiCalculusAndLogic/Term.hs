{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeSynonymInstances #-}

module Data.PiCalculusAndLogic.Term where

import Data.SymbolForChan

data Term = Empt | Fail

          -- Operators for classic pi calculus
          | FPUB { questToCommPipe    :: Chan, useBindFromPast     :: Chan }    -- use bind from past     quest < bfp >
          | BTRS { listenFromCommPipe :: Chan, newBindToRightSide  :: Chan }    -- bind to right side     listen ( newBindTRS )

          -- Operators for offer choosing
          | UBFF { useBindFromFuture  :: Chan, chooseAlterFromPipe :: Chan }    -- use bind from future   ( bff ) choose
          | LSBT { newBindToLeftSide  :: Chan, offerAlterToPipe    :: Chan }    -- bind to left side      < newBindTLS > offer

          -- Operators for channels
          | COMM [Term] -- listen quests, choose offers
          | ORDR [Term] -- create binds use binds, check order

          -- Operators for exponentials
          | SERV  Term  -- term can be removed or duplicated
          | LOG2  Term  -- not remove term only if place with it reduced 2^n times

          -- Operators for logic

          -- With channel communication
          | OPTF {                                 termToRemoveOrKeep :: Term } -- optional term for remove on fail
          | OPTS { termForSuccessOrRemove :: Term, termToRemoveOrKeep :: Term } -- optional term for remove after success

          | PATT [Term] -- if less term in order is success after fail of previous terms all terms is removed except this one
          | POOL [Term] -- greather terms in order must use duplication less than first in order
          
          -- Without channel communication between terms inside this operators
          | PLUS [Term] -- success one or more to success, other term is not forces to remove for this time
          | CONJ [Term] -- quick fail one or more to fail, success is not forced to check for this time
          | PROD [Term] -- success only if every term is succesefull
          | DISJ [Term] -- success only if every term is fail but only one is success

          -- Security operators

          | ISOL  Term  -- isolate all channel communications between terms inside PATT POOL ORDR in first layer
                        --                                               or inside OPTF OPTS(tfsor) second layer

          | MOBI  Term  -- all channel communication must be mobile, input  channel must get output channel only
                        --                                       and output channel must put input  channel only

          -- Foreign terms and tools
          | PROX       Dynamic -- proxy for dynamic from haskell
          | DBUG  Term Dynamic -- use dynamics for term debugging


 deriving (Eq,Ord)










