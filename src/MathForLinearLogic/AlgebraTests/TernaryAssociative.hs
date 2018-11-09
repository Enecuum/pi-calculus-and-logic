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

randomRewrite 0 (Op1 (Op1 a b c) d e) = Just (Op1 a (Op1 b c d) e)
randomRewrite 1 (Op1 (Op1 a b c) d e) = Just (Op1 a b (Op1 c d e))
randomRewrite 2 (Op1 a (Op1 b c d) e) = Just (Op1 (Op1 a b c) d e)
randomRewrite 3 (Op1 a (Op1 b c d) e) = Just (Op1 a b (Op1 c d e))
randomRewrite 4 (Op1 a b (Op1 c d e)) = Just (Op1 (Op1 a b c) d e)
randomRewrite 5 (Op1 a b (Op1 c d e)) = Just (Op1 a (Op1 b c d) e)
randomRewrite _ _ = Nothing

permute 0 (Op1 a b c) = (Op1 b c a)
permute 1 (Op1 a b c) = (Op1 c a b)
permute 2 (Op1 a b c) = (Op1 b a c)
permute 3 (Op1 a b c) = (Op1 a c b)
permute 4 (Op1 a b c) = (Op1 c b a)
permute _ a = a

randomPermute a = do
  w <- randomRIO (0 :: Int, 4)
  let f = permute w
  return (cata f a)

cata f (Op1 a b c) = f (Op1 (cata f a) (cata f b) (cata f c))
cata f a = f a

randomRewriteTerm :: TernAlg -> IO TernAlg
randomRewriteTerm b = do
  a <- randomPermute b
  w <- randomRIO (0 :: Int, 5)
  let b = randomRewrite w a
  case (b,a) of
    (Just (Op1 a b c),_) -> do a <- randomRewriteTerm a
                               b <- randomRewriteTerm b
                               c <- randomRewriteTerm c
                               return (Op1 a b c)
    (_,Op1 a b c) -> do  a <- randomRewriteTerm a
                         b <- randomRewriteTerm b
                         c <- randomRewriteTerm c
                         return (Op1 a b c)
    (_,a) -> return a

demo p = do
  a <- randomTerm p
  loop a
 where
  loop a = do a <- randomRewriteTerm a; clearScreen; setCursorPosition 0 0; print a; threadDelay 800000; loop a






