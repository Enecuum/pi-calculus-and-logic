{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeSynonymInstances #-}

module Data.PiCalculusAndLogic.Term where

import Data.SymbolForChan
import Data.Dynamic

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

          | Ordr [Term]
          | Comm [Term]

          -- Operators for exponentials
          | SERV  Term  -- term can be removed or duplicated
          | LOG2  Term  -- not remove term only if place with it reduced 2^n times

          -- Operators for logic

          -- With channel communication
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
          | DBUG  Term Dynamic -- use dynamics for term debugging
          | PROX       Dynamic -- pure functional proxy for dynamic from haskell
          | SEFF       Dynamic -- proxy for side effects

          -- Hinding tools
          | PRIO  Term Double  -- look to priority in search


 deriving (Eq,Ord,Show)

instance Ord Dynamic where
  compare _ _ = EQ

instance Eq Dynamic where
  (==) _ _ = True











