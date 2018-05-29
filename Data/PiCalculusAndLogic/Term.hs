{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeSynonymInstances #-}

module Data.PiCalculusAndLogic.Term where

import Data.SymbolForChan

data Term = Empt | Fail


          | FPUB { questToCommPipe    :: Chan, useBindFromPast     :: Chan }    -- use bind from past     quest < bfp >
          | BTRS { listenFromCommPipe :: Chan, newBindToRightSide  :: Chan }    -- bind to right side     listen ( newBindTRS )

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
          
          -- Without channel communication between terms inside this operators
          | PLUS [Term] -- success one or more to success
          | CONJ [Term] -- fail one or more to fail
          | PROD [Term] -- success only if every term is succesefull
          | DISJ [Term] -- success only if every term is fail but only one is success


 deriving (Eq,Ord)










