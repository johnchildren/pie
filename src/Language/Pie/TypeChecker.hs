{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Pie.TypeChecker
  ( alphaEquiv
  , synth
  , check
  , convert
  , val
  , ctxToEnvironment
  , tyInteract
  , readBackNorm
  , TypeError
  , Binding
  )
where


import           Data.Bifunctor                           ( first )
import           Control.Monad.Fail                       ( MonadFail
                                                          , fail
                                                          )
import           Language.Pie.Symbols                     ( VarName(..) )
import           Language.Pie.Environment                 ( Env )
import qualified Language.Pie.Environment      as Env
import           Language.Pie.Values                      ( Value(..)
                                                          , Neutral(..)
                                                          , Closure(..)
                                                          , Normal(..)
                                                          )
import qualified Language.Pie.Eval             as Eval
import           Language.Pie.Eval                        ( EvalError )
import           Language.Pie.Expr                        ( CoreExpr(..)
                                                          , Clos(..)
                                                          )

data TypeError = UnknownTypeError VarName
               | UnknownVariableError VarName
               | UnexpectedPiTypeError CoreExpr
               | UnificationError CoreExpr CoreExpr CoreExpr
               | TypeSynthesisError CoreExpr
               | NonPairError CoreExpr
               | NbEError EvalError
               | NotNormalisedError String
               deriving (Show, Eq)


val :: Env Value -> CoreExpr -> Either TypeError Value
val rho e = first NbEError $ Eval.val rho e

valOfClosure :: Closure -> Value -> Either TypeError Value
valOfClosure cl v = first NbEError $ Eval.valOfClosure cl v

-- read back a normalised value
readBackNorm :: Env Binding -> Normal -> CoreExpr
readBackNorm gamma (NormThe typ expr) = readBackNorm' typ expr
 where
  readBackNorm' VNat      VZero           = CZero
  readBackNorm' VNat (VAdd1 n) = CAdd1 (readBackNorm gamma (NormThe VNat n))
  --readBackNorm' (PI a b) f
  readBackNorm' VAtom     (VQuote x)      = CQuote x
  readBackNorm' VUniverse VNat            = CNat
  readBackNorm' VUniverse VAtom           = CAtom
  readBackNorm' VUniverse VUniverse       = CUniverse -- TODO: Not true
  --readBackNorm' VUniverse (VPair a b) =
  --  Pair (readBackNorm gamma (NormThe VUniverse a)) (readBackNorm gamma (NormThe VUniverse b))
  --readBackNorm' (VPair a b) (VCons c d) =
  --  Cons (readBackNorm gamma (NormThe a c)) (readBackNorm gamma (NormThe b d))
  readBackNorm' _         (VNeutral _ ne) = readBackNeutral gamma ne
  readBackNorm' t v =
    error $ "can't read back type: " ++ show t ++ "; value: " ++ show v

readBackNeutral :: Env Binding -> Neutral -> CoreExpr
readBackNeutral gamma = readBackNeutral'
 where
  readBackNeutral' (NVar x) = CVar x
  readBackNeutral' (NAp ne rand) =
    CApp (readBackNeutral gamma ne) (readBackNorm gamma rand)
  readBackNeutral' (NCar ne               ) = CCar (readBackNeutral gamma ne)
  readBackNeutral' (NCdr ne               ) = CCdr (readBackNeutral gamma ne)
  readBackNeutral' (NWhichNat ne base step) = CWhichNat
    (readBackNeutral gamma ne)
    (readBackNorm gamma base)
    (Clos (readBackNorm gamma step))
  readBackNeutral' (NIterNat ne base step) = CWhichNat
    (readBackNeutral gamma ne)
    (readBackNorm gamma base)
    (Clos (readBackNorm gamma step))
  readBackNeutral' (NRecNat ne base step) = CWhichNat
    (readBackNeutral gamma ne)
    (readBackNorm gamma base)
    (Clos (readBackNorm gamma step))

-- CoreExpressions that have no arguments to their constructor
-- TODO: I'm sure there's a better word for this?
isAtomic :: CoreExpr -> Bool
isAtomic CAtom     = True
isAtomic CNat      = True
isAtomic CZero     = True
isAtomic CUniverse = True
isAtomic _         = False

type Bindings = Env VarName

data Binding = Definition { _type :: Value, _value :: Value }
             | FreeVar { _type :: Value }

-- generate a symbol not in either environment
freshen :: Bindings -> Bindings -> VarName
freshen _ _ = VarName "foo"

alphaEquiv :: CoreExpr -> CoreExpr -> Bool
alphaEquiv e1 e2 = alphaEquiv' e1 e2 Env.empty Env.empty

alphaEquiv' :: CoreExpr -> CoreExpr -> Bindings -> Bindings -> Bool
alphaEquiv' e1 e2 _ _ | e1 == e2 && isAtomic e1 = True
alphaEquiv' (CVar v1) (CVar v2) (Env.lookup v1 -> Nothing) (Env.lookup v2 -> Nothing)
  = v1 == v2
alphaEquiv' (CVar v1) (CVar v2) (Env.lookup v1 -> Just e3) (Env.lookup v2 -> Just e4)
  = e3 == e4
alphaEquiv' (CVar   _ ) (CVar   _ ) _    _    = False
alphaEquiv' (CQuote a1) (CQuote a2) _    _    = a1 == a2
alphaEquiv' (CAdd1  n1) (CAdd1  n2) env1 env2 = alphaEquiv' n1 n2 env1 env2
alphaEquiv' (CLambda x (Clos b1)) (CLambda y (Clos b2)) xs1 xs2 =
  let fresh = freshen xs1 xs2
  in  let bigger1 = Env.insert x fresh xs1
          bigger2 = Env.insert y fresh xs2
      in  alphaEquiv' b1 b2 bigger1 bigger2
alphaEquiv' (CPi x a1 (Clos b1)) (CPi y a2 (Clos b2)) xs1 xs2 =
  alphaEquiv' a1 a2 xs1 xs2
    && let fresh = freshen xs1 xs2
       in  let bigger1 = Env.insert x fresh xs1
               bigger2 = Env.insert y fresh xs2
           in  alphaEquiv' b1 b2 bigger1 bigger2
alphaEquiv' _ _ _ _ = False

lookupType :: VarName -> Env Binding -> Either TypeError Value
lookupType name ctx = case Env.lookup name ctx of
  Nothing               -> Left $ UnknownVariableError name
  Just (FreeVar t     ) -> Right t
  Just (Definition _ t) -> Right t


instance MonadFail (Either TypeError) where
  fail = Left . NotNormalisedError

synth :: Env Binding -> CoreExpr -> Either TypeError CoreExpr
synth gamma = synth'
 where
  synth' :: CoreExpr -> Either TypeError CoreExpr
  synth' (CThe typ expr) = do
    tOut <- check gamma typ VUniverse
    tVal <- val (ctxToEnvironment gamma) tOut
    eOut <- check gamma expr tVal
    Right $ CThe tOut eOut
  synth' CUniverse             = Right $ CThe CUniverse CUniverse
--  synth' (CLambda x (Clos d)) = do
    -- TODO: fresh name for arrow
--    let xTyVal = VNeutral VUniverse (NVar (VarName "a"))
--    let xVal   = VNeutral xTyVal (NVar x)
--    (CThe dTy dVal) <- synth
--      (extendCtx (extendCtx gamma x xVal) (VarName "a") xTyVal)
--      d
--    let xTy = readBackNorm gamma (NormThe VUniverse xTyVal)
--    Right $ The (CArrow xTy dTy) (CLambda x (Clos dVal))
  synth' (CSigma x a (Clos d)) = do
    aOut <- check gamma a VUniverse
    aVal <- val (ctxToEnvironment gamma) aOut
    dOut <- check (extendCtx gamma x aVal) d VUniverse
    Right $ CThe CUniverse (CSigma x aOut (Clos dOut))
  synth' (CCar pr) = do
    (CThe prTy prOut) <- synth gamma pr
    prTyVal           <- val (ctxToEnvironment gamma) prTy
    case prTyVal of
      (VSigma a _) ->
        Right $ CThe (readBackNorm gamma (NormThe VUniverse a)) (CCar prOut)
      other ->
        Left $ NonPairError (readBackNorm gamma (NormThe VUniverse other))
  synth' (CCdr pr) = do
    (CThe prTy prOut) <- synth gamma pr
    prTyVal           <- val (ctxToEnvironment gamma) prTy
    case prTyVal of
      --(SIGMA _ d) -> Right $ The (readBackNorm gamma (NormThe VUniverse d)) (Car prOut)
      other ->
        Left $ NonPairError (readBackNorm gamma (NormThe VUniverse other))
  synth' CNat               = Right $ CThe CUniverse CNat
  synth' (CPi x a (Clos b)) = do
    aOut <- check gamma a VUniverse
    aVal <- val (ctxToEnvironment gamma) aOut
    bOut <- check (extendCtx gamma x aVal) b VUniverse
    Right $ CThe CUniverse (CPi x aOut (Clos bOut))
  -- Atom is a Universe
  synth' CAtom             = Right $ CThe CUniverse CAtom
  synth' (CApp rator rand) = do
    (CThe ratorT ratorOut) <- synth gamma rator
    ratorTVal              <- val (ctxToEnvironment gamma) ratorT
    case ratorTVal of
      (VPi a b) -> do
        randOut     <- check gamma rand a
        randOutVal  <- val (ctxToEnvironment gamma) randOut
        randOutClos <- valOfClosure b randOutVal
        Right
          $ CThe (readBackNorm gamma (NormThe VUniverse randOutClos)) ratorOut
      other -> Left
        (UnexpectedPiTypeError (readBackNorm gamma (NormThe VUniverse other)))
  synth' q@(CQuote _) = Right $ CThe CAtom q
  -- Zero is a Nat
  synth' CZero        = Right $ CThe CNat CZero
  -- if n in a@(Add1 n) is a Nat, a is a Nat
  synth' a@(CAdd1 n)  = do
    _ <- check gamma n VNat
    Right $ CThe CNat a
--  synth' (CCons a b) = do
--    (CThe aTy aVal) <- synth gamma a
--    (CThe bTy bVal) <- synth gamma b
--    Right $ CThe (CPair aTy bTy) (CCons aVal bVal)
  synth' (CVar x) = do
    t <- lookupType x gamma
    Right $ CThe (readBackNorm gamma (NormThe VUniverse t)) (CVar x)
  synth' other = Left $ TypeSynthesisError other


check :: Env Binding -> CoreExpr -> Value -> Either TypeError CoreExpr
check gamma = check'
 where
  check' :: CoreExpr -> Value -> Either TypeError CoreExpr
  check' (CCons a1 d1) (VSigma a2 d2) = do
    aOut  <- check gamma a1 a2
    aVal  <- val (ctxToEnvironment gamma) aOut
    aClos <- valOfClosure d2 aVal
    dOut  <- check gamma d1 aClos
    Right $ CCons aOut dOut
  check' CZero     VNat = Right CZero
  check' (CAdd1 n) VNat = do
    nOut <- check gamma n VNat
    Right $ CAdd1 nOut
  check' (CLambda x (Clos b)) (VPi a c) = do
    let xVal = VNeutral a (NVar x)
    cClos <- valOfClosure c xVal
    bOut  <- check (extendCtx gamma x a) b cClos
    Right $ CLambda x (Clos bOut)
  check' (CQuote s) VAtom = Right $ CQuote s
  check' e          t     = do
    (CThe tOut eOut) <- synth gamma e
    tVal             <- val (ctxToEnvironment gamma) tOut
    _                <- convert gamma VUniverse t tVal
    Right eOut

ctxToEnvironment :: Env Binding -> Env Value
ctxToEnvironment = Env.mapWithVarName aux
 where
  aux :: VarName -> Binding -> Value
  aux name (FreeVar t     ) = VNeutral t (NVar name)
  aux _    (Definition _ v) = v

extendCtx :: Env Binding -> VarName -> Value -> Env Binding
extendCtx ctx name v = Env.insert name (FreeVar v) ctx

convert
  :: Env Binding
  -> Value
  -> Value
  -> Value
  -> Either TypeError (CoreExpr, CoreExpr)
convert gamma t v1 v2 = do
  let e1 = readBackNorm gamma (NormThe t v1)
  let e2 = readBackNorm gamma (NormThe t v2)
  if alphaEquiv e1 e2
    then Right (e1, e2)
    else Left
      $ UnificationError e1 (readBackNorm gamma (NormThe VUniverse t)) e2


-- please ignore this hack
tyInteract :: Env Binding -> CoreExpr -> IO ()
tyInteract gamma e = case synth gamma e of
  Right (CThe ty expr) -> do
    let rho = ctxToEnvironment gamma
    print ty
    case val rho ty of
      Right tyVal -> case val rho expr of
        Right exprVal -> print $ readBackNorm gamma (NormThe tyVal exprVal)
        Left  err     -> print err
      Left err -> print err
  Right _   -> putStrLn "unexpected result from synth"
  Left  err -> print err
