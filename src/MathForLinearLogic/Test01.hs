{-# LANGUAGE FlexibleInstances #-}

module MathForLinearLogic.Test01 where

import Data.Complex
import System.IO.Unsafe
import System.Random

op02 :: Complex Double -> Complex Double -> Complex Double
op02 a b = one / ( one / a + one / b )

one = 1

{-
{-# NOINLINE one #-}
one = unsafePerformIO $ do
  a <- randomRIO (0.99,1.01)
  b <- randomRIO (-0.01,0.01)
  return (a :+ b)
-}

op01 :: Complex Double -> Complex Double -> Complex Double
op01 a b = exp ( log a `op02` log b )

a % b = op01 a b
a & b = op02 a b

a :: Complex Double
a = 5

b :: Complex Double
b = 7

c :: Complex Double
c = 11

d :: Complex Double
d = 13

e :: Complex Double
e = 17

f :: Complex Double
f = 19

