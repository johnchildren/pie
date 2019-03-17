{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Pie.TypeChecker
  ( alphaEquiv
  , synth
  , check
  , convert
  , val
  , ctxToEnvironment
  , TypeError
  , Binding
  )
where


import           Language.Pie.Symbols                     ( VarName(..) )
import           Language.Pie.Environment                 ( Env )
import qualified Language.Pie.Environment      as Env
import           Language.Pie.Values                      ( Value(..)
                                                          , Neutral(..)
                                                          , Closure(..)
                                                          , Normal(..)
                                                          )
import           Language.Pie.Expr                        ( Expr(..) )


valOfClosure :: Closure -> Value -> Either TypeError Value
valOfClosure (CLOS rho x e) v = val (Env.insert x v rho) e

val :: Env Value -> Expr -> Either TypeError Value
val env = val'
 where
  val' :: Expr -> Either TypeError Value
  val' (The _ expr)     = val env expr
  val' Universe         = Right UNI
  val' (Pie x a b  )    = (\y -> PI y (CLOS env x b)) <$> val env a
  val' (Lambda x b )    = Right $ LAM (CLOS env x b)
  val' (Sigma x a d)    = (\y -> SIGMA y (CLOS env x d)) <$> val env a
  val' (Pair a d   )    = PAIR <$> val env a <*> val env d
  val' (Cons a d   )    = CONS <$> val env a <*> val env d
  val' (Car pr     )    = doCar <$> val env pr
  val' (Cdr pr     )    = val env pr >>= doCdr
  val' Nat              = Right NAT
  val' Zero             = Right ZERO
  val' (Add1 n)         = ADD1 <$> val env n
  val' Atom             = Right ATOM
  val' (Quote ad      ) = Right $ QUOTE ad
  val' (App rator rand) = do
    ratorVal <- val env rator
    randVal  <- val env rand
    appd     <- doApp ratorVal randVal
    Right appd
  -- ind-Nat
  val' (Var x) = case Env.lookup x env of
    Just v  -> Right v
    Nothing -> Left $ UnknownVariableError x

doCar :: Value -> Value
doCar (PAIR a           _ ) = a
doCar (NEU  (SIGMA a _) ne) = NEU a (NCar ne)

doCdr :: Value -> Either TypeError Value
doCdr (  PAIR _           d ) = Right d
doCdr v@(NEU  (SIGMA _ d) ne) = do
  dClos <- valOfClosure d (doCar v)
  Right $ NEU dClos (NCdr ne)

doApp :: Value -> Value -> Either TypeError Value
doApp (LAM c) arg = valOfClosure c arg
doApp (NEU (PI a b) ne) arg =
  (\y -> NEU y (NAp ne (THE a arg))) <$> valOfClosure b arg

readBackNorm :: Env Binding -> Normal -> Expr
readBackNorm gamma (THE typ expr) = readBackNorm' typ expr
 where
  readBackNorm' NAT  ZERO       = Zero
  readBackNorm' NAT  (ADD1  n)  = Add1 (readBackNorm gamma (THE NAT n))
  --readBackNorm' (PI a b) f 
  readBackNorm' ATOM (QUOTE x)  = Quote x
  readBackNorm' UNI  NAT        = Nat
  readBackNorm' UNI  ATOM       = Atom
  readBackNorm' UNI  UNI        = Universe -- TODO: Not true
  readBackNorm' _    (NEU _ ne) = readBackNeutral gamma ne
  readBackNorm' t v =
    error $ "can't read back type: " ++ show t ++ "; value: " ++ show v

readBackNeutral :: Env Binding -> Neutral -> Expr
readBackNeutral gamma = readBackNeutral'
 where
  readBackNeutral' (NVar x) = Var x
  readBackNeutral' (NAp ne rand) =
    App (readBackNeutral gamma ne) (readBackNorm gamma rand)
  readBackNeutral' (NCar ne               ) = Car (readBackNeutral gamma ne)
  readBackNeutral' (NCdr ne               ) = Cdr (readBackNeutral gamma ne)
  readBackNeutral' (NWhichNat ne base step) = WhichNat
    (readBackNeutral gamma ne)
    (readBackNorm gamma base)
    (readBackNorm gamma step)
  readBackNeutral' (NIterNat ne base step) = WhichNat
    (readBackNeutral gamma ne)
    (readBackNorm gamma base)
    (readBackNorm gamma step)
  readBackNeutral' (NRecNat ne base step) = WhichNat
    (readBackNeutral gamma ne)
    (readBackNorm gamma base)
    (readBackNorm gamma step)

-- Expressions that have no arguments to their constructor
-- TODO: I'm sure there's a better word for this?
isAtomic :: Expr -> Bool
isAtomic Atom     = True
isAtomic Nat      = True
isAtomic Zero     = True
isAtomic Universe = True
isAtomic _        = False

type Bindings = Env VarName

data Binding = Definition { _type :: Value, _value :: Value }
             | FreeVar { _type :: Value }

-- generate a symbol not in either environment
freshen :: Bindings -> Bindings -> VarName
freshen _ _ = VarName "foo"

alphaEquiv :: Expr -> Expr -> Bool
alphaEquiv e1 e2 = alphaEquiv' e1 e2 Env.empty Env.empty

alphaEquiv' :: Expr -> Expr -> Bindings -> Bindings -> Bool
alphaEquiv' e1 e2 _ _ | e1 == e2 && isAtomic e1 = True
alphaEquiv' (Var v1) (Var v2) (Env.lookup v1 -> Nothing) (Env.lookup v2 -> Nothing)
  = v1 == v2
alphaEquiv' (Var v1) (Var v2) (Env.lookup v1 -> Just e3) (Env.lookup v2 -> Just e4)
  = e3 == e4
alphaEquiv' (Var   _ ) (Var   _ ) _    _    = False
alphaEquiv' (Quote a1) (Quote a2) _    _    = a1 == a2
alphaEquiv' (Add1  n1) (Add1  n2) env1 env2 = alphaEquiv' n1 n2 env1 env2
alphaEquiv' (Lambda x b1) (Lambda y b2) xs1 xs2 =
  let fresh = freshen xs1 xs2
  in  let bigger1 = Env.insert x fresh xs1
          bigger2 = Env.insert y fresh xs2
      in  alphaEquiv' b1 b2 bigger1 bigger2
alphaEquiv' (Pie x a1 b1) (Pie y a2 b2) xs1 xs2 =
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

data TypeError = UnknownTypeError VarName
               | UnknownVariableError VarName
               | UnexpectedPiTypeError Expr
               | UnificationError Expr Expr
               | TypeSynthesisError Expr
               | NonPairError Expr
               deriving (Show, Eq)

synth :: Env Binding -> Expr -> Either TypeError Expr
synth gamma = synth'
 where
  synth' :: Expr -> Either TypeError Expr
  synth' (The typ expr) = do
    tOut <- check gamma typ UNI
    tVal <- val (ctxToEnvironment gamma) tOut
    eOut <- check gamma expr tVal
    pure $ The tOut eOut
  synth' Universe   = pure $ The Universe Universe
  synth' (Pair a b) = do
    aOut <- check gamma a UNI
    bOut <- check gamma b UNI
    pure $ The Universe (Pair aOut bOut)
  synth' (Sigma x a d) = do
    aOut <- check gamma a UNI
    aVal <- val (ctxToEnvironment gamma) aOut
    dOut <- check (extendCtx gamma x aVal) d UNI
    pure $ The Universe (Sigma x aOut dOut)
  synth' (Car pr) = do
    (The prTy prOut) <- synth gamma pr
    prTyVal          <- val (ctxToEnvironment gamma) prTy
    case prTyVal of
      (SIGMA a _) -> pure $ The (readBackNorm gamma (THE UNI a)) (Car prOut)
      (PAIR  a _) -> pure $ The (readBackNorm gamma (THE UNI a)) (Car prOut)
      other       -> Left $ NonPairError (readBackNorm gamma (THE UNI other))
  synth' (Cdr pr) = do
    (The prTy prOut) <- synth gamma pr
    prTyVal          <- val (ctxToEnvironment gamma) prTy
    case prTyVal of
      --(SIGMA _ d) -> pure $ The (readBackNorm gamma (THE UNI d)) (Car prOut)
      (PAIR _ b) -> pure $ The (readBackNorm gamma (THE UNI b)) (Car prOut)
      other      -> Left $ NonPairError (readBackNorm gamma (THE UNI other))
  synth' Nat         = pure $ The Universe Nat
  synth' (Pie x a b) = do
    aOut <- check gamma a UNI
    aVal <- val (ctxToEnvironment gamma) aOut
    bOut <- check (extendCtx gamma x aVal) b UNI
    pure $ The Universe (Pie x aOut bOut)
  synth' Atom             = pure $ The Universe Atom
  synth' (App rator rand) = do
    (The ratorT ratorOut) <- synth gamma rator
    ratorTVal             <- val (ctxToEnvironment gamma) ratorT
    case ratorTVal of
      (PI a b) -> do
        randOut     <- check gamma rand a
        randOutVal  <- val (ctxToEnvironment gamma) randOut
        randOutClos <- valOfClosure b randOutVal
        pure $ The (readBackNorm gamma (THE UNI randOutClos)) ratorOut
      other ->
        Left (UnexpectedPiTypeError (readBackNorm gamma (THE UNI other)))
  synth' (Var x) = do
    t <- lookupType x gamma
    pure $ The (readBackNorm gamma (THE UNI t)) (Var x)
  synth' other = Left $ TypeSynthesisError other


check :: Env Binding -> Expr -> Value -> Either TypeError Expr
check gamma = check'
 where
  check' :: Expr -> Value -> Either TypeError Expr
  check' (Cons a1 d1) (PAIR a2 d2) = do
    aOut <- check gamma a1 a2
    dOut <- check gamma d1 d2
    pure $ Cons aOut dOut
  check' (Cons a1 d1) (SIGMA a2 d2) = do
    aOut  <- check gamma a1 a2
    aVal  <- val (ctxToEnvironment gamma) aOut
    aClos <- valOfClosure d2 aVal
    dOut  <- check gamma d1 aClos
    pure $ Cons aOut dOut
  check' Zero     NAT = pure Zero
  check' (Add1 n) NAT = do
    nOut <- check gamma n NAT
    pure $ Add1 nOut
--  check' (Lambda x b) (PIE a c) = 
  check' (Quote s) ATOM = pure $ Quote s
  check' e         t    = do
    (The tOut eOut) <- synth gamma e
    tVal            <- val (ctxToEnvironment gamma) tOut
    _               <- convert gamma UNI t tVal
    pure eOut

ctxToEnvironment :: Env Binding -> Env Value
ctxToEnvironment = Env.mapWithVarName aux
 where
  aux :: VarName -> Binding -> Value
  aux name (FreeVar t     ) = NEU t (NVar name)
  aux _    (Definition _ v) = v

extendCtx :: Env Binding -> VarName -> Value -> Env Binding
extendCtx ctx name v = Env.insert name (FreeVar v) ctx

convert
  :: Env Binding -> Value -> Value -> Value -> Either TypeError (Expr, Expr)
convert gamma t v1 v2 = do
  let e1 = readBackNorm gamma (THE t v1)
  let e2 = readBackNorm gamma (THE t v2)
  if alphaEquiv e1 e2
    then Right (e1, e2)
    else Left $ UnificationError (readBackNorm gamma (THE UNI t)) e2
