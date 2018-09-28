
import Control.Concurrent.STM.TVar
import System.IO.Unsafe
import System.Random
import Data.Maybe

data Term
 = MROOT -- Multiplicative root of top formula, expresion must be equal to multiplicative unit for success
 | AUNIT -- Additive unit, a+0 == a
 | MUNIT -- Multiplicative unit, a*1 == a, or for example matrix * identitiy matrix for non commutative case
 | Term `CMCNJ` Term -- Commutative Multiplicative Conjunction
 | Term `CMDSJ` Term -- Commutative Multiplicative Disjunction
 | Term `NMCNJ` Term -- Non-Commutative Associative Multiplicative Conjunction (like matrix multiplication for example)
 | Term `CADSJ` Term -- Commutative Additive Disjunction
 | Term `CACNJ` Term -- Commutative Additive Conjunction
 | INVRT Term -- Multiplicative inverse, a^-1 or like matrix inverse for non commutative
 | NEGAT Term -- Additive negatation, negative numbers for example
 | OFCRS Term -- Of-course exponential operator
 | WHYNT Term -- Why-not exponential operator

 | Share UniqueShare
 | Focus Term

 | Atom Descr

 deriving (Show,Eq)

-- predicate -> termFunctorM -> originalTerm -> m transformatedTerm
termMapM :: Monad m => (Term -> Bool) -> (Term -> m Term) -> Term -> m Term
termMapM p f o@(a `CMCNJ` b) | p o = do c <- f a; d <- f b; return (c `CMCNJ` d)
termMapM p f o@(a `CMDSJ` b) | p o = do c <- f a; d <- f b; return (c `CMDSJ` d)
termMapM p f o@(a `NMCNJ` b) | p o = do c <- f a; d <- f b; return (c `NMCNJ` d)
termMapM p f o@(a `CADSJ` b) | p o = do c <- f a; d <- f b; return (c `CADSJ` d)
termMapM p f o@(a `CACNJ` b) | p o = do c <- f a; d <- f b; return (c `CACNJ` d)
termMapM p f o@(INVRT a) | p o = do b <- f a; return (INVRT b)
termMapM p f o@(NEGAT a) | p o = do b <- f a; return (NEGAT b)
termMapM p f o@(OFCRS a) | p o = do b <- f a; return (OFCRS b)
termMapM p f o@(WHYNT a) | p o = do b <- f a; return (WHYNT b)
termMapM _ _ o = return o

toBinop (CMCNJ a b) = Just ((a,b),(CMCNJ,INVRT))
toBinop (CMDSJ a b) = Just ((a,b),(CMDSJ,INVRT))
toBinop (NMCNJ a b) = Just ((a,b),(NMCNJ,INVRT))
toBinop (CADSJ a b) = Just ((a,b),(CADSJ,NEGAT))
toBinop (CACNJ a b) = Just ((a,b),(CACNJ,NEGAT))
toBinop _ = Nothing

termCataM :: Monad m => (Term -> Bool) -> (Term -> m Term) -> Term -> m Term
termCataM p f a = termMapM p (termCataM p f) a >>= f

termAnaM :: Monad m => (Term -> Bool) -> (Term -> m Term) -> Term -> m Term
termAnaM p f a = f a >>= termMapM p (termAnaM p f)

data UniqueShare = USH Integer

instance Show UniqueShare where
  show (USH a) = "sh" ++ show (a `mod` 99)

instance Eq UniqueShare where
  USH a == USH b = a == b

newUniqueShare :: IO Term
newUniqueShare = do
  a <- randomRIO (2^80,2^90)
  return $ Share $ USH a

data Pattern = ConstName String | ConstId Integer | UniqueId Integer | UniqueWildcard Integer
 deriving (Eq)

data Descr   = Descr { path :: [Pattern], isReduced :: TVar (Maybe Integer) }
 deriving (Eq)

instance Show Descr where
  show (Descr a b) = concat $ map (':':) $ map f a
   where
    f (UniqueWildcard _) = "*"
    f (ConstName a) = a
    f (UniqueId a) = "uid" ++ show a
    f (ConstId a) = "id" ++ show a

at x = unsafePerformIO $ do
  tv <- newTVarIO Nothing
  return $ Atom $ Descr [ConstName x] tv

rotateFocus [] = []

rotateFocus [(a,b)] | isJust c = case d of
  (Focus g, Focus h) -> unsafePerformIO $ do
    shH <- newUniqueShare
    let rot01 = rotateFocus [(g, e b $ f shH)]
    let rot02 = rotateFocus [(h, shH)] -- term h equal to shH in sense of connected shares
    return $ rot01 ++ rot02
  (Focus g, h) -> rotateFocus [(g, e b $ f h)]
  (g, Focus h) -> rotateFocus [(h, e (f g) h)]
 where
  c = toBinop a
  Just (d,(e,f)) = c

rotateFocus (a:b) = rotateFocus [a] ++ rotateFocus b

{-


 конструкции

 a * b * a^-1
 c^-1 * d * c


 -}







