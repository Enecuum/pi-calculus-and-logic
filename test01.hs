
import Control.Concurrent.STM.TVar
import System.IO.Unsafe
import System.Random
import Data.Maybe
import Data.Functor.Identity
import Control.Monad.State.Lazy

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

tupleMap f (a,b) = (f a, f b)

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
termMapM p f o@(Focus a) | p o = do b <- f a; return (Focus b)
termMapM _ _ o = return o

toBinop (CMCNJ a b) = Just ((a,b),(CMCNJ,INVRT))
toBinop (CMDSJ a b) = Just ((a,b),(CMDSJ,INVRT))
toBinop (NMCNJ a b) = Just ((a,b),(NMCNJ,INVRT))
toBinop (CADSJ a b) = Just ((a,b),(CADSJ,NEGAT))
toBinop (CACNJ a b) = Just ((a,b),(CACNJ,NEGAT))
toBinop _ = Nothing

-- predicate -> termAlgebraM -> originalTerm -> m transformatedTerm
termCataM :: Monad m => (Term -> Bool) -> (Term -> m Term) -> Term -> m Term
termCataM p f a = termMapM p (termCataM p f) a >>= f

-- predicate -> termCoAlgebraM -> originalTerm -> m transformatedTerm
termAnaM :: Monad m => (Term -> Bool) -> (Term -> m Term) -> Term -> m Term
termAnaM p f a = f a >>= termMapM p (termAnaM p f)

-- predicate -> termAlgebraM -> termCoAlgebraM -> originalTerm -> m transformatedTerm
termHyloM :: Monad m => (Term -> Bool) -> (Term -> m Term) -> (Term -> m Term) -> Term -> m Term
termHyloM p f g a = g a >>= termMapM p (termHyloM p f g) >>= f

-- predicate -> termGAlgebraM -> originalTerm -> m transformatedTerm
termParaM :: Monad m => (Term -> Bool) -> ((Term, Term) -> m Term) -> Term -> m Term
termParaM p f a = termMapM p (termParaM p f) a >>= curry f a

termApoM :: Monad m => (Term -> Bool) -> (Term -> m (Either Term Term)) -> Term -> m Term
termApoM p f a = do
  b <- f a
  case b of
    Left  c -> return c
    Right d -> termMapM p (termApoM p f) d

termCata p f   a = runIdentity $ termCataM p (return . f) a
termAna  p f   a = runIdentity $ termAnaM  p (return . f) a
termHylo p f g a = runIdentity $ termHyloM p (return . f) (return . g) a
termPara p f   a = runIdentity $ termParaM p (return . f) a
termApo  p f   a = runIdentity $ termApoM  p (return . f) a

isFocusExistInTermOp :: Term -> Bool
isFocusExistInTermOp a = snd $ runState (termMapM p f a) False
 where
  p (Focus _) = False
  p        _  = True
  f :: Term -> State Bool Term
  f o@(Focus _) = put True >> return o
  f o           =             return o

class FocusUp a where
  focusUp :: a -> a

instance FocusUp Term where
  focusUp = termCata (const True) f
   where
    f a | isFocusExistInTermOp a = Focus a
    f a = a

focup01 a = map (tupleMap focusUp) a

test01 = [(at "q" `CMCNJ` (Focus (at "w") `CMCNJ` at "e"), at "r" `CMCNJ` at "t")]
test02 = [(at "q" `CMCNJ` (Focus (at "w") `CMCNJ` Focus (at "e")), at "r" `CMCNJ` at "t")]

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

rotateFocus [(Focus a,b)] = rotateFocus [(a,b)]

rotateFocus o@[(a,b)] | isJust c = case d of
  (Focus g, Focus h) -> unsafePerformIO $ do
    shH <- newUniqueShare
    let rot01 = rotateFocus [(g, e b $ f shH)]
    let rot02 = rotateFocus [(h, shH)] -- term h equal to shH in sense of connected shares
    return $ rot01 ++ rot02
  (Focus g, h) -> rotateFocus [(g, e b $ f h)]
  (g, Focus h) -> rotateFocus [(h, e (f g) b)]
  _ -> o
 where
  c = toBinop a
  Just (d,(e,f)) = c

rotateFocus (a:b:c) = rotateFocus [a] ++ rotateFocus (b:c)

rotateFocus a = a

{-


 конструкции

 a * b * a^-1
 c^-1 * d * c


 -}







