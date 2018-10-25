{-# LANGUAGE OverloadedStrings #-}

module Data.PiCalculusAndLogic.Presets where

import Data.SymbolForChan
import Data.PiCalculusAndLogic.Term
import Data.PiCalculusAndLogic.Tools

{-
testLambda = 
  Serv ( Wait "myLambdaServer" "lambdaChan"
    <> Serv ( Wait "lambdaChan" "arg1"
           <> Wait "lambdaChan" "arg2"
           <> Wait "lambdaChan" "arg3"
           <> ( Send "isToken"     "arg1"
             *| Send "isHole"      "arg2"
             *| Send "isBlackHole" "arg3" )
           <> Send "lambdaChan" 1
            )
     )

isClosedForWait =
    Serv ( Wait "isClosedForReceive" 102 <> (
           OptF ( Wait 102 2 <> Send 103 1 )
        *| OptW 99 <> Wait 103 2 <> Send 99 1 <> Fail
        *| OptW 99 <> Send 99 1
         ) )

addTokensServer =
  Serv ( Wait "addTokensServer" "args"
      <> Serv ( Wait "args" "arg"
             <> Send "isTokenServer" "arg"
             <> Serv ( Wait "arg" 2 <> Send "args" 1 ) )
       )

mulTokensServer =
  Serv ( Wait "mulTokensServer" "args"
      <> ( Serv ( Wait "args" "arg"
               <> Send "isTokenServer" "arg"
               <> Serv ( Wait "arg" 2 <> Log2 ( Send "logt" 1 ) ) )
        *| Serv ( Log2 ( Wait "logt" 2 ) <> Send "args" 1 )
         )
       )
-}

