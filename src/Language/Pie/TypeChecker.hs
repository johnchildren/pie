{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Language.Pie.TypeChecker
  ( synth
  , check
  , convert
  , val
  , ctxToEnvironment
  , readBackNorm
  , TypeError(..)
  , Binding(..)
  )
where


import           Prelude
import           Control.Applicative                      ( liftA2
                                                          , liftA3
                                                          )
import           Control.Effect                           ( Carrier
                                                          , Member
                                                          , run
                                                          )
import           Control.Effect.Fresh                     ( Fresh
                                                          , fresh
                                                          )
import           Control.Effect.Error                     ( Error
                                                          , throwError
                                                          , runError
                                                          )
import           Control.Effect.Reader                    ( Reader
                                                          , ask
                                                          , local
                                                          , runReader
                                                          )
import           Language.Pie.Symbols                     ( VarName
                                                            ( VarName
                                                            , Dimmed
                                                            )
                                                          )
import           Language.Pie.Environment                 ( Env )
import qualified Language.Pie.Environment      as Env
import           Language.Pie.Values                      ( Value(..)
                                                          , Neutral(..)
                                                          , Closure(..)
                                                          , Normal(..)
                                                          , closName
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
               | ReadBackError Value Value
               deriving (Show, Eq)

val
  :: ( Member (Error TypeError) sig
     , Member (Reader (Env Binding)) sig
     , Carrier sig m
     )
  => CoreExpr
  -> m Value
val e = do
  gamma <- ask
  let rho = ctxToEnvironment gamma
  case run . runError . runReader rho $ Eval.val e of
    Left  err -> throwError (NbEError err)
    Right v   -> pure v

valOfClosure
  :: (Member (Error TypeError) sig, Carrier sig m)
  => Closure
  -> Value
  -> m Value
valOfClosure cl v = case run . runError $ Eval.valOfClosure cl v of
  Left  err -> throwError (NbEError err)
  Right res -> pure res

doCar :: (Member (Error TypeError) sig, Carrier sig m) => Value -> m Value
doCar v = case run . runError $ Eval.doCar v of
  Left  err -> throwError (NbEError err)
  Right res -> pure res

doCdr :: (Member (Error TypeError) sig, Carrier sig m) => Value -> m Value
doCdr v = case run . runError $ Eval.doCdr v of
  Left  err -> throwError (NbEError err)
  Right res -> pure res

doApp
  :: (Member (Error TypeError) sig, Carrier sig m) => Value -> Value -> m Value
doApp f v = case run . runError $ Eval.doApp f v of
  Left  err -> throwError (NbEError err)
  Right res -> pure res


type TypeCheckEffects sig
  = ( Member (Error TypeError) sig
    , Member Fresh sig
    , Member (Reader (Env Binding)) sig
    )

-- read back a normalised value
readBackNorm :: (TypeCheckEffects sig, Carrier sig m) => Normal -> m CoreExpr
readBackNorm (NormThe typ expr) = readBackNorm' typ expr
 where
  readBackNorm'
    :: (TypeCheckEffects sig, Carrier sig m) => Value -> Value -> m CoreExpr
  readBackNorm' VNat      VZero     = pure CZero
  readBackNorm' VNat      (VAdd1 n) = CAdd1 <$> readBackNorm' VNat n
  readBackNorm' (VPi a b) f         = do
    let x = closName b
    y <- freshen x
    let yVal = VNeutral a (NVar y)
    fVal  <- doApp f yVal
    fTy   <- valOfClosure b yVal
    fExpr <- local (\g -> extendCtx g y a) $ readBackNorm (NormThe fTy fVal)
    pure $ CLambda y (Clos fExpr)
  readBackNorm' (VSigma a d) p = do
    theCar <- doCar p
    theCdr <- doCdr p
    dTy    <- valOfClosure d theCdr
    aExpr  <- readBackNorm' a theCar
    dExpr  <- readBackNorm' dTy theCdr
    pure $ CCons aExpr dExpr
  readBackNorm' VAtom     (VQuote x) = pure $ CQuote x
  readBackNorm' VUniverse VNat       = pure CNat
  readBackNorm' VUniverse VAtom      = pure CAtom
  readBackNorm' VUniverse (VList xs) =
    CList <$> readBackNorm (NormThe VUniverse xs)
  readBackNorm' VUniverse (VSigma a b) = readBackSigmaPi CSigma a b
  readBackNorm' VUniverse (VPi    a b) = readBackSigmaPi CPi a b
  readBackNorm' (VList _) VNil         = pure CNil
  readBackNorm' l@(VList lTy) (VListExp x xs) =
    CListExp <$> readBackNorm' lTy x <*> readBackNorm' l xs
  readBackNorm' VUniverse VUniverse       = pure CUniverse -- TODO: Not true
  readBackNorm' _         (VNeutral _ ne) = readBackNeutral ne
  readBackNorm' t         v               = throwError $ ReadBackError t v

readBackSigmaPi
  :: (TypeCheckEffects sig, Carrier sig m)
  => (VarName -> CoreExpr -> Clos -> CoreExpr)
  -> Value
  -> Closure
  -> m CoreExpr
readBackSigmaPi constructor a b = do
  let x = closName b
  y       <- freshen x
  closVal <- valOfClosure b (VNeutral a (NVar y))
  aExpr   <- readBackNorm (NormThe VUniverse a)
  bExpr   <- local (\g -> extendCtx g y a)
    $ readBackNorm (NormThe VUniverse closVal)
  pure $ constructor y aExpr (Clos bExpr)

--readBackListBody :: Value -> Value -> m CoreExpr
--readBackListBody x xs = CListExp (readBackNorm x)


readBackNeutral
  :: (TypeCheckEffects sig, Carrier sig m) => Neutral -> m CoreExpr
readBackNeutral (NVar x) = pure $ CVar x
readBackNeutral (NAp ne rand) =
  CApp <$> readBackNeutral ne <*> readBackNorm rand
readBackNeutral (NCar ne) = CCar <$> readBackNeutral ne
readBackNeutral (NCdr ne) = CCdr <$> readBackNeutral ne
readBackNeutral (NWhichNat ne base step) =
  CWhichNat
    <$> readBackNeutral ne
    <*> readBackNorm base
    <*> (Clos <$> readBackNorm step)
readBackNeutral (NIterNat ne base step) =
  CIterNat
    <$> readBackNeutral ne
    <*> readBackNorm base
    <*> (Clos <$> readBackNorm step)
readBackNeutral (NRecNat ne base step) =
  CRecNat
    <$> readBackNeutral ne
    <*> readBackNorm base
    <*> (Clos <$> readBackNorm step)

-- CoreExpressions that have no arguments to their constructor
-- TODO: I'm sure there's a better word for this?
isAtomic :: CoreExpr -> Bool
-- AtomSame-Atom
isAtomic CAtom     = True
-- NatSame-Nat
isAtomic CNat      = True
-- NatSame-Zero
isAtomic CZero     = True
-- ListSame-nil
isAtomic CNil      = True
isAtomic CUniverse = True
isAtomic _         = False

type Bindings = Env VarName

data Binding = Definition { _type :: Value, _value :: Value }
             | FreeVar { _type :: Value }

freshen :: (Member Fresh sig, Carrier sig m) => VarName -> m VarName
freshen (VarName sym _) = VarName sym <$> fresh
freshen (Dimmed  sym _) = Dimmed sym <$> fresh

-- generate a symbol not in either environment
freshen2 :: (Applicative m) => m VarName
freshen2 = pure $ VarName "foo" 0

liftA4
  :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = liftA3 f a b c <*> d

-- TODO: should be able to check that they have the same type as well
runAlphaEquiv :: CoreExpr -> CoreExpr -> Bool
runAlphaEquiv e1 e2 =
  run . runReader (Env.empty :: Bindings, Env.empty :: Bindings) $ alphaEquiv
    e1
    e2

alphaEquiv
  :: (Member (Reader (Bindings, Bindings)) sig, Carrier sig m)
  => CoreExpr
  -> CoreExpr
  -> m Bool
alphaEquiv e1 e2 | e1 == e2 && isAtomic e1 = pure True
alphaEquiv (CVar v1) (CVar v2)             = do
  (env1, env2) <- ask @(Bindings, Bindings)
  case (Env.lookup v1 env1, Env.lookup v2 env2) of
    (Nothing, Nothing) -> pure $ v1 == v2
    (Just e3, Just e4) -> pure $ e3 == e4
    _                  -> pure False
-- AtomSame-Tick
alphaEquiv (CQuote a1          ) (CQuote a2          ) = pure $ a1 == a2
-- NatSame-add1
alphaEquiv (CAdd1  n1          ) (CAdd1  n2          ) = alphaEquiv n1 n2
alphaEquiv (CLambda x (Clos b1)) (CLambda y (Clos b2)) = do
  freshened <- freshen2
  local
      (\(env1, env2) ->
        (Env.insert x freshened env1, Env.insert y freshened env2)
      )
    $ alphaEquiv b1 b2
alphaEquiv (CPi x a1 (Clos b1)) (CPi y a2 (Clos b2)) = do
  freshened <- freshen2
  liftA2
    (&&)
    (alphaEquiv a1 a2)
    ( local
        (\(env1, env2) ->
          (Env.insert x freshened env1, Env.insert y freshened env2)
        )
    $ alphaEquiv b1 b2
    )
-- NatSame-w-N1
-- TODO: check type of _
alphaEquiv (CWhichNat CZero (CThe _ b1) _) b2 = alphaEquiv b1 b2
-- NatSame-w-N
alphaEquiv (CWhichNat t1 (CThe bTy1 b1) (Clos s1)) (CWhichNat t2 (CThe bTy2 b2) (Clos s2))
  = liftA4 (\w x y z -> w && x && y && z)
           (alphaEquiv t1 t2)
           (alphaEquiv bTy1 bTy2)
           (alphaEquiv b1 b2)
           (alphaEquiv s1 s2)
-- NatSame-w-N2
-- TODO: check types of b and s
alphaEquiv (CWhichNat (CAdd1 n1) (CThe bTy b) (Clos s1)) (CApp s2 n2) =
  liftA2 (&&) (alphaEquiv n1 n2) (alphaEquiv s1 s2)
alphaEquiv (CList x) (CList y) = alphaEquiv x y
alphaEquiv (CListExp x xs) (CListExp y ys) =
  liftA2 (&&) (alphaEquiv x y) (alphaEquiv xs ys)
alphaEquiv _ _ = pure False

lookupType
  :: ( Member (Error TypeError) sig
     , Member (Reader (Env Binding)) sig
     , Carrier sig m
     )
  => VarName
  -> m Value
lookupType name = do
  gamma <- ask
  case Env.lookup name gamma of
    Nothing               -> throwError $ UnknownVariableError name
    Just (FreeVar t     ) -> pure t
    Just (Definition t _) -> pure t


-- Construct a tuple of type and expression
synth
  :: (TypeCheckEffects sig, Carrier sig m) => CoreExpr -> m (CoreExpr, CoreExpr)
synth (CThe typ expr) = do
  tOut <- check typ VUniverse
  tVal <- val tOut
  eOut <- check expr tVal
  pure (tOut, eOut)
synth CUniverse             = pure (CUniverse, CUniverse)
synth (CSigma x a (Clos d)) = do
  aOut <- check a VUniverse
  aVal <- val aOut
  dOut <- local (\g -> extendCtx g x aVal) $ check d VUniverse
  pure (CUniverse, CSigma x aOut (Clos dOut))
synth (CCar pr) = do
  (prTy, prOut) <- synth pr
  prTyVal       <- val prTy
  case prTyVal of
    (VSigma a _) -> (, CCar prOut) <$> readBackNorm (NormThe VUniverse a)
    other        -> do
      otherVal <- readBackNorm (NormThe VUniverse other)
      throwError $ NonPairError otherVal
synth (CCdr pr) = do
  (prTy, prOut) <- synth pr
  prTyVal       <- val prTy
  case prTyVal of
    (VSigma _ d) -> do
      prOutVal <- val prOut
      theCar   <- doCar prOutVal
      closVal  <- valOfClosure d theCar
      ty       <- readBackNorm (NormThe VUniverse closVal)
      pure (ty, CCar prOut)
    other -> do
      otherVal <- readBackNorm (NormThe VUniverse other)
      throwError $ NonPairError otherVal
-- NatF
synth CNat               = pure (CUniverse, CNat)
synth (CPi x a (Clos b)) = do
  aOut <- check a VUniverse
  aVal <- val aOut
  bOut <- local (\g -> extendCtx g x aVal) $ check b VUniverse
  pure (CUniverse, CPi x aOut (Clos bOut))
-- AtomF
synth CAtom             = pure (CUniverse, CAtom)
synth (CApp rator rand) = do
  (ratorTy, ratorOut) <- synth rator
  ratorTyVal          <- val ratorTy
  case ratorTyVal of
    (VPi a b) -> do
      randOut     <- check rand a
      randOutVal  <- val randOut
      randOutClos <- valOfClosure b randOutVal
      tyOut       <- readBackNorm (NormThe VUniverse randOutClos)
      pure (tyOut, CApp ratorOut randOut)
    other -> do
      otherVal <- readBackNorm (NormThe VUniverse other)
      throwError $ UnexpectedPiTypeError otherVal
-- AtomI
synth q@(CQuote _) = pure (CAtom, q)
-- NatI-1
synth CZero        = pure (CNat, CZero)
-- NatI-2
synth a@(CAdd1 n)  = do
  _ <- check n VNat
  pure (CNat, a)
-- ListI-2
synth (CListExp e es) = do
  (eTy, eOut) <- synth e
  eTyVal      <- val eTy
  esOut       <- check es (VList eTyVal)
  pure (CList eTy, CListExp eOut esOut)
synth (CVar x) = do
  tVal <- lookupType x
  t    <- readBackNorm (NormThe VUniverse tVal)
  pure (t, CVar x)
synth (CWhichNat t b (Clos s)) = do
  gamma       <- ask
  tOut        <- check t VNat
  (bTy, bOut) <- synth b
  sOut <- check s (VPi VNat (CLOS (ctxToEnvironment gamma) (Dimmed "x" 0) bTy))
  pure (bTy, CWhichNat tOut (CThe bTy bOut) (Clos sOut))
synth other = throwError $ TypeSynthesisError other


check
  :: (TypeCheckEffects sig, Carrier sig m) => CoreExpr -> Value -> m CoreExpr
check (CCons a1 d1) (VSigma a2 d2) = do
  aOut  <- check a1 a2
  aVal  <- val aOut
  aClos <- valOfClosure d2 aVal
  dOut  <- check d1 aClos
  pure $ CCons aOut dOut
check CZero     VNat = pure CZero
check (CAdd1 n) VNat = do
  nOut <- check n VNat
  pure $ CAdd1 nOut
check (CLambda x (Clos b)) (VPi a c) = do
  let xVal = VNeutral a (NVar x)
  cClos <- valOfClosure c xVal
  bOut  <- local (\g -> extendCtx g x a) $ check b cClos
  pure $ CLambda x (Clos bOut)
check (CQuote s) VAtom     = pure $ CQuote s
-- ListI-1
check CNil       (VList _) = pure CNil
check e          t         = do
  (tOut, eOut) <- synth e
  tVal         <- val tOut
  _            <- convert VUniverse t tVal
  pure eOut

ctxToEnvironment :: Env Binding -> Env Value
ctxToEnvironment = Env.mapWithVarName aux
 where
  aux :: VarName -> Binding -> Value
  aux name (FreeVar t     ) = VNeutral t (NVar name)
  aux _    (Definition _ v) = v

extendCtx :: Env Binding -> VarName -> Value -> Env Binding
extendCtx ctx name v = Env.insert name (FreeVar v) ctx

convert
  :: (TypeCheckEffects sig, Carrier sig m)
  => Value
  -> Value
  -> Value
  -> m (CoreExpr, CoreExpr)
convert tv v1 v2 = do
  e1 <- readBackNorm (NormThe tv v1)
  e2 <- readBackNorm (NormThe tv v2)
  t  <- readBackNorm (NormThe VUniverse tv)
  if runAlphaEquiv e1 e2
    then pure (e1, e2)
    else throwError $ UnificationError e1 t e2
