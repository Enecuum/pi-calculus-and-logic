{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeSynonymInstances #-}

module Data.PiCalculusAndLogic.Term where

import Data.SymbolForChan

data Term = Empt | Fail


          | Ques { quesToPipe :: Chan, bindToSend :: Chan }  -- a < b >
          | Wait { chanToWait :: Chan, nameToBind :: Chan }  -- a ( b )   bindToRightSide


          | Send { nextToBind :: Chan, alterToSend :: Chan } -- ( a ) [ b ]
          | Recv { bindToWait :: Chan, nameToAlter :: Chan } -- < a > [ b ]  bindToLeftSide


-- FPUB
-- UBFF
-- BTRS
-- LSBT

          | FPUB { questToCommPipe    :: Chan, useBindFromPast     :: Chan } -- use bind from past     quest < bfp >
          | BTRS { listenFromCommPipe :: Chan, newBindToRightSide  :: Chan } -- bind to right side     listen ( newBindTRS )

          | UBFF { useBindFromFuture  :: Chan, chooseAlterFromPipe :: Chan } -- use bind from future   ( bff ) choose
          | LSBT { newBindToLeftSide  :: Chan, offerAlterToPipe    :: Chan } -- bind to left side      < newBindTLS > offer


          | UseBindFromPast   { questToCommPipe   :: Chan, useBindFromPastAndNameSendIt :: Chan }
          | UseBindFromFuture { useBindFromFuture :: Chan, chooseAlterForBindUse :: Chan }

          | BindToRightSide { chanToWait :: Chan, nameToBind  :: Chan }
          | BindToLeftSide  { bindToWait :: Chan, nameToAlter :: Chan }



          | OptW { chanToWait :: Chan }
          | OptF  Term
          | Comm [Term]
          | Ordr [Term]
          | Serv  Term
          | Log2  Term

 deriving (Eq,Ord)

{-

! test1 . a<d> . _b(f) . a<d> ;


! a ( _b ) , _b<c> . _b(e) ;


optional once in serve


-}









