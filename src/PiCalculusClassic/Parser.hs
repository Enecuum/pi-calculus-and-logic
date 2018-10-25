module PiCalculusClassic.Parser where

import PiCalculusClassic.Expr
import Catarotoid.Pattern

{-
pattern = do
  a <- letter
  b <- many alphaNum
  return $ [ConstName "PiCalculus", ConstName "Channel", ConstName "Normal", ConstName (a:b)]


send :: Parser Expr
send = do
  sps
  a <- pattern
  sps
  char '<'
  sps
  b <- option [ConstName "PiCalculus", ConstName "Channel", ConstName "Special", ConstName "BlackHole"] pattern
  sps
  char '>'
  sps
  char '.'
  sps
  c <- process
  return $ Send a b c
-}
