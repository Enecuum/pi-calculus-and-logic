{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, TypeFamilies, UndecidableInstances, ConstrainedClassMethods, AllowAmbiguousTypes, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}

module MathForLinearLogic.Matrix where

import Data.Matrix hiding (Matrix)
import qualified Data.Matrix as M
import System.Random
import Control.Monad
import Data.List
import Math.NumberTheory.Primes.Factorisation
import System.Random.Shuffle
import Math.NumberTheory.Primes.Testing
import GHC.TypeLits
import Data.Proxy

type Matrix7det32type01 = Matrix 7 4194476831

newtype Matrix (size :: Nat) (prime :: Nat) = Matrix (Vec size (Vec size Integer))

data a :. b = a :. b

infixr 8 :.

type family Vec (a :: Nat) b where
  Vec 0 a = ()
  Vec 1 a = a :. ()
  Vec n a = a :. Vec (n-1) a

type family CellNum a where
  CellNum (Matrix a b) = Div (b+1) a

type family DiagCellNum a where
  DiagCellNum a = CellNum a - 1

test :: Vec 10 Integer
test = undefined

test01 :: Proxy (CellNum (Matrix7det32type01))
test01 = undefined

test02 :: Proxy (DiagCellNum (Matrix7det32type01))
test02 = undefined

test03 :: Vec 4 Double
test03 = 1 :. 2 :. 3 :. 4 :. ()

class VecToList a b where
  vecToList :: a -> [b]

instance VecToList () b where
  vecToList () = []

instance VecToList b a => VecToList (a :. b) a where
  vecToList (a :. b) = a : vecToList b

instance (VecToList b a, VecToList (a :. b) a, Show a) => Show (a :. b) where
  show (a :. b) = show ( a : vecToList b )

--matrixToLists :: (KnownNat a, KnownNat b, VecToList (Vec a (Vec a Integer)) (Vec a Integer), VecToList (Vec a Integer) Integer) => Matrix a b -> [[Integer]]
matrixToTab :: (KnownNat a, KnownNat b, VecToList (Vec a (Vec a Integer)) (Vec a Integer)) => Matrix a b -> [Vec a Integer]
matrixToTab (Matrix a) = vecToList a

matrixToLists :: (KnownNat a, KnownNat b, VecToList (Vec a Integer) Integer, VecToList (Vec a (Vec a Integer)) (Vec a Integer)) => Matrix a b -> [[Integer]]
matrixToLists a = map vecToList $ matrixToTab a

toDataMatrix a = fromLists $ matrixToLists a

testMatrix :: Matrix 2 127
testMatrix = Matrix (  (1 :. 2 :. ())  :.  (3 :. 4 :. ()) :. () )

instance (KnownNat a, KnownNat b, VecToList (Vec a Integer) Integer, VecToList (Vec a (Vec a Integer)) (Vec a Integer)) => Show (Matrix a b) where
  show a = show $ toDataMatrix a

{-

*MathForLinearLogic.Matrix> randomCyclicMatrixPrime 32 7
(31.96584373130438,(4194476831,599210975),4194476831)
(31.96584373130438,(4194476831,599210975))

-}


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

{-
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
-}

