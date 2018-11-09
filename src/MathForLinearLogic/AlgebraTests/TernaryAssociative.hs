module MathForLinearLogic.AlgebraTests.TernaryAssociative where

import System.Random
import System.Console.ANSI
import Control.Concurrent

data TernAlg = Atom Integer | Op1 TernAlg TernAlg TernAlg
 deriving (Show)

randomAtom = do
  a <- randomRIO (2^8,2^10)
  return $ Atom a

randomTerm p = do
  a <- randomRIO (0 :: Double,p)
  let q = p * 0.9
  if a < 0.5 then randomAtom
             else do b <- randomTerm q
                     c <- randomTerm q
                     d <- randomTerm q
                     return $ Op1 b c d

{-
randomRewrite 0 (Op1 

randomRewrite w (Op1 a (Op1 b c)) |     w = Just (Op1 (Op1 a b) c)
randomRewrite w (Op1 (Op1 a b) c) | not w = Just (Op1 a (Op1 b c))
randomRewrite _ _ = Nothing

randomRewriteTerm :: TernAlg -> IO TernAlg
randomRewriteTerm a = do
  w <- randomRIO (False,True)
  let b = randomRewrite w a
  case (b,a) of
    (Just (Op1 a b),_) -> do a <- randomRewriteTerm a
                             b <- randomRewriteTerm b
                             return (Op1 a b)
    (_,Op1 a b) -> do  a <- randomRewriteTerm a
                       b <- randomRewriteTerm b
                       return (Op1 a b)
    (_,a) -> return a

demo p = do
  a <- randomTerm p
  loop a
 where
  loop a = do a <- randomRewriteTerm a; clearScreen; setCursorPosition 0 0; print a; threadDelay 800000; loop a
  --loop a = do a <- randomRewriteTerm a; clearScreen; setCursorPosition 0 0; print a; threadDelay 400000; loop a
-}






