module Language.Pie.Eval
  ( EvalError(..)
  , val
  , valOfClosure
  )
where

import           Control.Monad                            ( join )
import           Data.Functor.Foldable                    ( Base
                                                          , cata
                                                          )
import           Language.Pie.Environment                 ( Env )
import           Language.Pie.Expr                        ( Expr(..)
                                                          , ExprF(..)
                                                          , Clos(..)
                                                          )

import           Language.Pie.Symbols                     ( VarName(..) )
import qualified Language.Pie.Environment      as Env
import           Language.Pie.Values                      ( Value(..)
                                                          , Neutral(..)
                                                          , Closure(..)
                                                          , Normal(..)
                                                          )

type Algebra t a = Base t a -> a

data EvalError = UnknownVariableError VarName
               | InvalidCar Value
               | InvalidCdr Value
               | InvalidApp Value Value
               | InvalidWhichNat Expr Value Value
               | InvalidIterNat Expr Value Value
               | InvalidRecNat Expr Value Value
  deriving (Show, Eq)

valOfClosure :: Closure -> Value -> Either EvalError Value
valOfClosure (CLOS rho x e) v = val (Env.insert x v rho) e

val :: Env Value -> Expr -> Either EvalError Value
val rho = cata val'
 where
  val' :: Algebra Expr (Either EvalError Value)
  val' (TheF _ expr)         = expr
  val' UniverseF             = Right UNI
  val' (ArrowF a d         ) = ARROW <$> a <*> d
  val' (PiF x a (Clos b)   ) = (\y -> PI y (CLOS rho x b)) <$> a
  val' (LambdaF x (Clos b) ) = Right $ LAM (CLOS rho x b)
  val' (SigmaF x a (Clos d)) = (\y -> SIGMA y (CLOS rho x d)) <$> a
  val' (PairF a d          ) = PAIR <$> a <*> d
  val' (ConsF a d          ) = CONS <$> a <*> d
  val' (CarF pr            ) = doCar =<< pr
  val' (CdrF pr            ) = doCdr =<< pr
  val' NatF                  = Right NAT
  val' ZeroF                 = Right ZERO
  val' (Add1F n)             = ADD1 <$> n
  val' AtomF                 = Right ATOM
  val' (QuoteF ad      )     = Right $ QUOTE ad
  val' (AppF rator rand)     = join $ doApp <$> rator <*> rand
  val' (WhichNatF target base step) =
    join $ doWhichNat rho step <$> target <*> base
  val' (IterNatF target base step) =
    join $ doIterNat rho step <$> target <*> base
  val' (RecNatF target base step) =
    join $ doRecNat rho step <$> target <*> base
  val' (VarF x) = case Env.lookup x rho of
    Just v  -> Right v
    Nothing -> Left $ UnknownVariableError x

doCar :: Value -> Either EvalError Value
doCar (CONS a           _ ) = Right a
doCar (PAIR a           _ ) = Right a
doCar (NEU  (SIGMA a _) ne) = Right $ NEU a (NCar ne)
doCar v                     = Left $ InvalidCar v

doCdr :: Value -> Either EvalError Value
doCdr (  CONS _           d ) = Right d
doCdr (  PAIR _           d ) = Right d
doCdr v@(NEU  (SIGMA _ d) ne) = do
  aVal  <- doCar v
  dClos <- valOfClosure d aVal
  Right $ NEU dClos (NCdr ne)
doCdr v = Left $ InvalidCdr v

doApp :: Value -> Value -> Either EvalError Value
doApp (LAM c) arg = valOfClosure c arg
doApp (NEU (PI a b) ne) arg =
  (\y -> NEU y (NAp ne (THE a arg))) <$> valOfClosure b arg
doApp v1 v2 = Left $ InvalidApp v1 v2


doWhichNat :: Env Value -> Clos -> Value -> Value -> Either EvalError Value
doWhichNat _ _ ZERO base = Right base
doWhichNat rho (Clos (Lambda x (Clos e))) (ADD1 n) _ =
  doApp (LAM (CLOS rho x e)) n
doWhichNat _ (Clos step) v base = Left $ InvalidWhichNat step v base

doIterNat :: Env Value -> Clos -> Value -> Value -> Either EvalError Value
doIterNat _ _ ZERO base = Right base
doIterNat rho step@(Clos (Lambda x (Clos e))) (ADD1 n) base =
  doApp (LAM (CLOS rho x e)) =<< doIterNat rho step n base
doIterNat _ (Clos step) v base = Left $ InvalidIterNat step v base

doRecNat :: Env Value -> Clos -> Value -> Value -> Either EvalError Value
doRecNat _   _                               ZERO     base = Right base
doRecNat rho step@(Clos (Lambda x (Clos e))) (ADD1 n) base = do
  rator <- doApp (LAM (CLOS rho x e)) n
  doApp rator =<< doRecNat rho step n base
doRecNat _ (Clos step) v base = Left $ InvalidRecNat step v base
