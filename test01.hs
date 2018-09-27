
import Control.Concurrent.STM.TVar
import System.IO.Unsafe

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

{-


 конструкции

 a * b * a^-1
 c^-1 * d * c


 -}







