module Data.PiCalculusAndLogic.Parser where

import Text.Parsec
import Text.Parsec.String

import Data.SymbolForChan
import Data.PiCalculusAndLogic.Term


channel :: Parser Chan
channel = do
  a <- letter
  b <- many alphaNum
  return $ fromString (a:b)

term = fail <|> empt <|> block

block :: Parser Term
block = undefined

send :: Parser Term
send = undefined

wait :: Parser Term
wait = undefined

comm :: Parser Term
comm = undefined

ordr :: Parser Term
ordr = undefined

serv :: Parser Term
serv = undefined

optw :: Parser Term
optw = undefined

optf :: Parser Term
optf = undefined

empt :: Parser Term
empt = undefined

fail :: Parser Term
fail = undefined

log2 :: Parser Term
log2 = undefined



