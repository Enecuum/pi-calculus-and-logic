{-# LANGUAGE ViewPatterns #-}

module Data.PiCalculusAndLogic.Reduction where

import Data.PiCalculusAndLogic.Term
import System.Random

type Seed = Int

norm :: Term -> Term
norm a = a

reduce :: Seed -> (Term,Seed) -> (Term,Seed)

reduce sa (COMM (FPUB a b : ORDR (BTRS c d : e) : f),sb) | a == c = (norm $ COMM [replaceBindR b d (ORDR e), g], sc)
 where (g,sc) = reduce (sa+1) (ORDR f,sb+1)

replaceBindR a b (ORDR (FPUB c d : e))
   | b == c && b == d = norm $ ORDR [FPUB a a, f]
   | b == c           = norm $ ORDR [FPUB a d, f]
   |           b == d = norm $ ORDR [FPUB c a, f]
   | otherwise        = norm $ ORDR [FPUB c d, f]
 where f = replaceBindR a b (ORDR e)

replaceBindR a b (ORDR (BTRS c d : e))
   | b == c    = norm $ ORDR [BTRS a a, f]
   | otherwise = norm $ ORDR [BTRS c d, f]
 where f = replaceBindR a b (ORDR e)

replaceBindL a b (ORDR (reverse -> (UBFF c d : e)))
   | b == c && b == d = norm $ ORDR [f, UBFF a a]
   | b == c           = norm $ ORDR [f, UBFF a d]
   |           b == d = norm $ ORDR [f, UBFF c a]
   | otherwise        = norm $ ORDR [f, UBFF c d]
 where f = replaceBindL a b (ORDR e)

replaceBindL a b (ORDR (reverse -> (LSBT c d : e)))
   | b == c    = norm $ ORDR [f, LSBT a a]
   | otherwise = norm $ ORDR [f, LSBT c d]
 where f = replaceBindL a b (ORDR e)
  
  

