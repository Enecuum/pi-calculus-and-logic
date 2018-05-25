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

term = comm <|> ordr <|> serv <|> optf <|> fail <|> empt <|> log2 <|> block

block :: Parser Term
block = undefined

pattern :: Parser (Either String Chan)
pattern = (do a <- oneOf "*?"; return $ Left a) <|> (do a <- channel; return $ Right a)

send :: Parser Term
send = do
  ma <- optionMaybe channel
  char '<'
  mb <- optionMaybe pattern
  char '>'
  case (ma,mb) of
    (Nothing,Nothing)         -> parserZero
    (Just  a,Just (Left "?")) -> parserZero
    (Just  a,Just (Left "*")) -> parserZero
    (Nothing,Just (Left "?")) -> parserZero
    (Nothing,Just (Left "*")) -> parserZero
    (Nothing,Just (Right  b)) -> parserZero
    (Just  a,Just (Right  b)) -> Send a b

wait :: Parser Term
wait = do
  ma <- optionMaybe channel

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



