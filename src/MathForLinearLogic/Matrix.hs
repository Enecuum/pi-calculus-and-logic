module MathForLinearLogic.Matrix where

import Data.Matrix
import System.Random
import Control.Monad
import Data.List
import Math.NumberTheory.Primes.Factorisation
import System.Random.Shuffle
import Math.NumberTheory.Primes.Testing


rotateList :: [a] -> [a]
rotateList (x:xs) = xs ++ [x]

randomNat :: Double -> IO Integer
randomNat scale = do
  a <- randomRIO (0,1)
  let b = log ( (1+a*a) / (1-a) )
  if isNaN b || isInfinite b then randomNat scale else do
     let c = round $ b*(2**scale)
     d <- randomRIO (0,c)
     return $ max 1 d

randomCyclicMatrixPrime :: Double -> Int -> IO (Double,(Integer,Integer))
randomCyclicMatrixPrime scale size = do
  let a = 2**scale
  let b = round (a / realToFrac size)
  c <- randomRIO (b`div`2,b*2)
  let det = (c+1) * toInteger size - 1
  let list = replicate (size-1) (c+1) ++ [c]
  let tab = reverse $ take size $ iterate rotateList list
  let det2 = detLaplace $ fromLists tab
  case (size `mod` 2 == 1, det > 1 && isPrime det, det == det2, factorise det) of
    (False,_,_,_) -> error "invalid size"
    (_,True,_,_) -> print (log (realToFrac det) / log 2, (det, c), det2) >> return (log (realToFrac det) / log 2, (det, c))
    --(_,True,_,_) -> print (det2) >> return (log (realToFrac det) / log 2, (det, c))
    _ -> randomCyclicMatrixPrime scale size

randomCyclicMatrix :: Double -> Int -> IO (Matrix Integer)
randomCyclicMatrix scale size = do
  let a = 2**scale
  let b = round (a / realToFrac size)
  c <- randomRIO (b`div`2,b*2)
  --a <- randomNat scale
  let list = replicate (size-1) c ++ [c-1]
  --list <- mapM (\_ -> randomNat scale) [1..size]
  --list <- mapM (\_ -> randomRIO (900,999)) [1..size]
  let tab = reverse $ take size $ iterate rotateList list
  return $ fromLists tab

randomPrimeMatrix :: Double -> Int -> IO (Double,Integer,Integer)
randomPrimeMatrix scale size = do
  a <- randomCyclicMatrix scale size
  let b = detLaplace a
  case (b > 1, isPrime b, factorise b) of
    (True,True,_) -> return (log (realToFrac b) / log 2, b, head $ head $ toLists a)
    --(True,_,[(_,1)]) -> print b >> return a
    _ -> randomPrimeMatrix scale size

