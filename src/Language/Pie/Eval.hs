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
import           Language.Pie.Expr                        ( Clos(..)
                                                          , CoreExpr(..)
                                                          , CoreExprF(..)
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
               | InvalidWhichNat CoreExpr Value Value
               | InvalidIterNat CoreExpr Value Value
               | InvalidRecNat CoreExpr Value Value
  deriving (Show, Eq)

valOfClosure :: Closure -> Value -> Either EvalError Value
valOfClosure (CLOS rho x e) v = val (Env.insert x v rho) e

val :: Env Value -> CoreExpr -> Either EvalError Value
val rho = cata val'
 where
  val' :: Algebra CoreExpr (Either EvalError Value)
  val' (CTheF _ expr)         = expr
  val' CUniverseF             = Right VUniverse
  val' (CPiF x a (Clos b)   ) = (\y -> VPi y (CLOS rho x b)) <$> a
  val' (CLambdaF x (Clos b) ) = Right $ VLambda (CLOS rho x b)
  val' (CSigmaF x a (Clos d)) = (\y -> VSigma y (CLOS rho x d)) <$> a
  val' (CConsF a d          ) = VCons <$> a <*> d
  val' (CCarF pr            ) = doCar =<< pr
  val' (CCdrF pr            ) = doCdr =<< pr
  val' CNatF                  = Right VNat
  val' CZeroF                 = Right VZero
  val' (CAdd1F n)             = VAdd1 <$> n
  val' CAtomF                 = Right VAtom
  val' (CQuoteF ad      )     = Right $ VQuote ad
  val' (CAppF rator rand)     = join $ doApp <$> rator <*> rand
  val' (CWhichNatF target base step) =
    join $ doWhichNat rho step <$> target <*> base
  val' (CIterNatF target base step) =
    join $ doIterNat rho step <$> target <*> base
  val' (CRecNatF target base step) =
    join $ doRecNat rho step <$> target <*> base
  val' (CVarF x) = case Env.lookup x rho of
    Just v  -> Right v
    Nothing -> Left $ UnknownVariableError x

doCar :: Value -> Either EvalError Value
doCar (VCons    a            _ ) = Right a
doCar (VNeutral (VSigma a _) ne) = Right $ VNeutral a (NCar ne)
doCar v                          = Left $ InvalidCar v

doCdr :: Value -> Either EvalError Value
doCdr (  VCons    _            d ) = Right d
doCdr v@(VNeutral (VSigma _ d) ne) = do
  aVal  <- doCar v
  dClos <- valOfClosure d aVal
  Right $ VNeutral dClos (NCdr ne)
doCdr v = Left $ InvalidCdr v

doApp :: Value -> Value -> Either EvalError Value
doApp (VLambda c) arg = valOfClosure c arg
doApp (VNeutral (VPi a b) ne) arg =
  (\y -> VNeutral y (NAp ne (NormThe a arg))) <$> valOfClosure b arg
doApp v1 v2 = Left $ InvalidApp v1 v2


doWhichNat :: Env Value -> Clos -> Value -> Value -> Either EvalError Value
doWhichNat _ _ VZero base = Right base
doWhichNat rho (Clos (CLambda x (Clos e))) (VAdd1 n) _ =
  doApp (VLambda (CLOS rho x e)) n
doWhichNat _ (Clos step) v base = Left $ InvalidWhichNat step v base

doIterNat :: Env Value -> Clos -> Value -> Value -> Either EvalError Value
doIterNat _ _ VZero base = Right base
doIterNat rho step@(Clos (CLambda x (Clos e))) (VAdd1 n) base =
  doApp (VLambda (CLOS rho x e)) =<< doIterNat rho step n base
doIterNat _ (Clos step) v base = Left $ InvalidIterNat step v base

doRecNat :: Env Value -> Clos -> Value -> Value -> Either EvalError Value
doRecNat _   _ VZero     base = Right base
doRecNat rho step@(Clos (CLambda x (Clos e))) (VAdd1 n) base = do
  rator <- doApp (VLambda (CLOS rho x e)) n
  doApp rator =<< doRecNat rho step n base
doRecNat _ (Clos step) v base = Left $ InvalidRecNat step v base
