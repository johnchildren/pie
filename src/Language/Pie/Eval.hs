{-# LANGUAGE FlexibleContexts #-}

module Language.Pie.Eval
  ( EvalError(..)
  , val
  , valOfClosure
  , doCar
  , doCdr
  , doApp
  )
where

import           Prelude
import           Control.Effect                           ( Carrier
                                                          , Member
                                                          )
import           Control.Effect.Reader                    ( Reader
                                                          , ask
                                                          , runReader
                                                          )
import           Control.Effect.Error                     ( Error
                                                          , throwError
                                                          )
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

valOfClosure
  :: (Member (Error EvalError) sig, Carrier sig m)
  => Closure
  -> Value
  -> m Value
valOfClosure (CLOS rho x e) v = runReader (Env.insert x v rho) $ val e

val
  :: ( Member (Error EvalError) sig
     , Member (Reader (Env Value)) sig
     , Carrier sig m
     )
  => CoreExpr
  -> m Value
val = cata val'
 where
  val'
    :: ( Member (Error EvalError) sig
       , Member (Reader (Env Value)) sig
       , Carrier sig m
       )
    => Algebra CoreExpr (m Value)
  val' (CTheF _ expr     ) = expr
  val' (CPiF x a (Clos b)) = do
    rho <- ask
    (\y -> VPi y (CLOS rho x b)) <$> a
  val' (CLambdaF x (Clos b)) = do
    rho <- ask
    pure $ VLambda (CLOS rho x b)
  val' (CSigmaF x a (Clos d)) = do
    rho <- ask
    (\y -> VSigma y (CLOS rho x d)) <$> a
  val' (CConsF a d)       = VCons <$> a <*> d
  val' (CCarF pr  )       = doCar =<< pr
  val' (CCdrF pr  )       = doCdr =<< pr
  val' CNatF              = pure VNat
  val' CZeroF             = pure VZero
  val' (CAdd1F n)         = VAdd1 <$> n
  val' CAtomF             = pure VAtom
  val' (CQuoteF ad      ) = pure $ VQuote ad
  val' (CAppF rator rand) = join $ doApp <$> rator <*> rand
  val' (CWhichNatF target base step) =
    join $ doWhichNat step <$> target <*> base
  val' (CIterNatF target base step) = join $ doIterNat step <$> target <*> base
  val' (CRecNatF  target base step) = join $ doRecNat step <$> target <*> base
  val' (CListF e                  ) = VList <$> e
  val' CNilF                        = pure VNil
  val' (CListExpF a d)              = VListExp <$> a <*> d
  val' (CVarF x      )              = do
    rho <- ask
    case Env.lookup x rho of
      Just v  -> pure v
      Nothing -> throwError $ UnknownVariableError x
  val' CUniverseF = pure VUniverse

doCar :: (Member (Error EvalError) sig, Carrier sig m) => Value -> m Value
doCar (VCons    a            _ ) = pure a
doCar (VSigma   a            _ ) = pure a -- TODO: Not sure if this is true
doCar (VNeutral (VSigma a _) ne) = pure $ VNeutral a (NCar ne)
doCar v                          = throwError $ InvalidCar v

doCdr :: (Member (Error EvalError) sig, Carrier sig m) => Value -> m Value
doCdr (  VCons    _            d ) = pure d
-- TODO: Cdr of sigma for non-neutral ?
doCdr v@(VNeutral (VSigma _ d) ne) = do
  aVal  <- doCar v
  dClos <- valOfClosure d aVal
  pure $ VNeutral dClos (NCdr ne)
doCdr v = throwError $ InvalidCdr v

doApp
  :: (Member (Error EvalError) sig, Carrier sig m) => Value -> Value -> m Value
doApp (VLambda c) arg = valOfClosure c arg
doApp (VNeutral (VPi a b) ne) arg =
  (\y -> VNeutral y (NAp ne (NormThe a arg))) <$> valOfClosure b arg
doApp v1 v2 = throwError $ InvalidApp v1 v2

doWhichNat
  :: ( Member (Error EvalError) sig
     , Member (Reader (Env Value)) sig
     , Carrier sig m
     )
  => Clos
  -> Value
  -> Value
  -> m Value
doWhichNat _ VZero base = pure base
doWhichNat (Clos (CLambda x (Clos e))) (VAdd1 n) _ =
  ask >>= \rho -> doApp (VLambda (CLOS rho x e)) n
doWhichNat (Clos step) v base = throwError $ InvalidWhichNat step v base

doIterNat
  :: ( Member (Error EvalError) sig
     , Member (Reader (Env Value)) sig
     , Carrier sig m
     )
  => Clos
  -> Value
  -> Value
  -> m Value
doIterNat _ VZero     base = pure base
doIterNat step@(Clos (CLambda x (Clos e))) (VAdd1 n) base = do
  rho <- ask
  doApp (VLambda (CLOS rho x e)) =<< doIterNat step n base
doIterNat (Clos step) v base = throwError $ InvalidIterNat step v base

doRecNat
  :: ( Member (Error EvalError) sig
     , Member (Reader (Env Value)) sig
     , Carrier sig m
     )
  => Clos
  -> Value
  -> Value
  -> m Value
doRecNat _ VZero     base = pure base
doRecNat step@(Clos (CLambda x (Clos e))) (VAdd1 n) base = do
  rho   <- ask
  rator <- doApp (VLambda (CLOS rho x e)) n
  doApp rator =<< doRecNat step n base
doRecNat (Clos step) v base = throwError $ InvalidRecNat step v base
