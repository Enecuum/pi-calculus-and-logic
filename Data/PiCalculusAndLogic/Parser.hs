module Data.PiCalculusAndLogic.Parser where

import Data.Char
import Data.List

import Text.Parsec
import Text.Parsec.String

import Data.PiCalculusAndLogic.Expr

sps :: Parser ()
sps = many space >> return ()

namespaceA :: Parser [String]
namespaceA = do
  let f = do a <- letter
             b <- many alphaNum
             return (a:b)
  sepBy1 f (char ':')

forbidden = "<>(){}:.|!?"

isAbbr a = any isUpper $ map head a

trim a = reverse d
 where
  b = concat $ map f $ group a
  f (a:_) | isSpace a = [a]
  f a = a
  c = dropWhile isSpace b
  d = dropWhile isSpace $ reverse c

namespaceB :: Parser [String]
namespaceB = do
  char '{'
  let f = do sps
             a <- lower
             b <- many $ noneOf forbidden
             return (a:b)
  a <- sepBy1 f (char ':')
  char '}'
  return $ map trim a


channel :: Parser Expr
channel = f <|> j
 where
  f = do a <- namespaceB; return $ Blck [] $ Left a
  j = do a <- namespaceA; return $ if isAbbr a then Abbr a else Chan a

star :: Parser Expr
star = do
  char '*'
  return Star

mark :: Parser Expr
mark = do
  char '?'
  return Mark

send :: Parser Expr
send = do
  sps
  ma <- optionMaybe $ channel
  sps
  char '<'
  sps
  mb <- optionMaybe $ channel <|> star
  sps
  char '>'
  sps
  conv (ma,mb)
 where
  conv (Nothing,Just Star) = parserZero
  conv (Just a,Nothing) = return $ Send a Toke
  conv (Nothing,Just b) = return $ Send Inhe b
  conv (Just a,Just  b) = return $ Send a    b
  conv _                = parserZero

wait :: Parser Expr
wait = do
  sps
  ma <- optionMaybe $ channel
  sps
  char '('
  sps
  mb <- optionMaybe $ channel <|> star <|> mark
  sps
  char ')'
  sps
  conv (ma,mb)
 where
  conv (Nothing,Just Star) = parserZero
  conv (Nothing,Just Mark) = parserZero
  conv (Just a,Nothing) = return $ Wait a Bind
  conv (Nothing,Just b) = return $ Send Inhe b
  conv (Just a,Just  b) = return $ Send a    b
  conv _                = parserZero

blck = do
  sps
  a <- optionMaybe namespaceA
  sps
  char '{'
  sps
  b <- exprC
  sps
  char '}'
  sps
  return $ Blck (f a) $ Right b
 where
  f Nothing  = []
  f (Just a) = a

exprA = try blck <|> try send <|> wait

exprB = try ordr <|> exprA

exprC = try comm <|> exprB


ordr :: Parser Expr
ordr = do
  sps
  a <- exprA
  sps
  char '.'
  sps
  b <- exprB
  sps
  return $ Ordr [a,b]

comm :: Parser Expr
comm = do
  sps
  a <- exprB
  sps
  char '|'
  sps
  b <- exprC
  sps
  return $ Comm [a,b]

{-
send :: Parser Expr
send = do
  sps
  ma <- optionMaybe channel
  char '<'
  mb <- optionMaybe $ (do a <- term; return $ Left a) <|> (do a <- pattern; return $ Right a)
  char '>'
  case (ma,mb) of
    (Nothing,Nothing)         -> parserZero
    (Just  a,Just (Left "?")) -> parserZero
    (Just  a,Just (Left "*")) -> parserZero
    (Nothing,Just (Left "?")) -> parserZero
    (Nothing,Just (Left "*")) -> parserZero
    (Nothing,Just (Right  b)) -> parserZero
    (Just  a,Just (Right  b)) -> Send a b
-}

{-
term = comm <|> ordr <|> serv <|> optf <|> fail <|> empt <|> log2 <|> block

block :: Parser Term
block = undefined

pattern :: Parser (Either String Chan)
pattern = (do a <- oneOf "*?"; return $ Left a) <|> (do a <- channel; return $ Right a)

wait :: Parser Term
wait = do
  ma <- optionMaybe channel

comm :: Parser Term
comm = undefined

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
-}



