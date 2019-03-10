{-# LANGUAGE ViewPatterns #-}

module TypeChecker
  ( alphaEquiv
  )
where

import           Language.Pie.Values                      ( Value(..)
                                                          , Closure(..)
                                                          )
import           Language.Pie.Expr                        ( Expr(..)
                                                          , VarName(..)
                                                          )
import           Language.Pie.Environment                 ( Env
                                                          , emptyEnv
                                                          , lookupEnv
                                                          , insertEnv
                                                          )


doCar :: Value -> Value
doCar = undefined

doCdr :: Value -> Value
doCdr = undefined

doApp :: Value -> Value -> Value
doApp = undefined

val :: Env -> Expr -> Value
val env = val'
 where
  val' :: Expr -> Value
  val' (The _ expr)     = val env expr
  val' Universe         = UNI
  val' (Pie x a b  )    = PI (val env a) (CLOS env x b)
  val' (Lambda x b )    = LAM (CLOS env x b)
  val' (Sigma x a d)    = SIGMA (val env a) (CLOS env x d)
  val' (Cons a d   )    = PAIR (val env a) (val env d)
  val' (Car pr     )    = doCar (val env pr)
  val' (Cdr pr     )    = doCdr (val env pr)
  val' Nat              = NAT
  val' Zero             = ZERO
  val' (Add1 n)         = ADD1 (val env n)
  val' AtomType         = ATOM
  val' (AtomData ad   ) = QUOTE ad
  val' (App rator rand) = doApp (val env rator) (val env rand)
  -- ind-Nat
  --val' (Var x) = lookupEnv env x

-- generate a symbol not in either environment
genSym :: Env -> Env -> VarName
genSym = undefined

-- Expressions that have no arguments to their constructor
-- TODO: I'm sure there's a better word for this?
isAtomic :: Expr -> Bool
isAtomic AtomType = True
isAtomic Nat      = True
isAtomic Zero     = True
isAtomic Universe = True
isAtomic _        = False

alphaEquiv :: Expr -> Expr -> Bool
alphaEquiv e1 e2 = alphaEquiv' e1 e2 emptyEnv emptyEnv

alphaEquiv' :: Expr -> Expr -> Env -> Env -> Bool
alphaEquiv' e1 e2 _ _ | e1 == e2 && isAtomic e1 = True
alphaEquiv' (Var v1) (Var v2) (lookupEnv v1 -> Nothing) (lookupEnv v2 -> Nothing)
  = v1 == v2
alphaEquiv' (Var v1) (Var v2) (lookupEnv v1 -> Just e3) (lookupEnv v2 -> Just e4)
  = e3 == e4
alphaEquiv' (Var      _ ) (Var      _ ) _ _ = False
alphaEquiv' (AtomData a1) (AtomData a2) _ _ = a1 == a2
alphaEquiv' (Lambda x b1) (Lambda y b2) xs1 xs2 =
  let fresh = genSym xs1 xs2
  in  let bigger1 = insertEnv x (Var fresh) xs1
          bigger2 = insertEnv y (Var fresh) xs2
      in  alphaEquiv' b1 b2 bigger1 bigger2
alphaEquiv' (Pie x a1 b1) (Pie y a2 b2) xs1 xs2 =
  alphaEquiv' a1 a2 xs1 xs2
    && let fresh = genSym xs1 xs2
       in  let bigger1 = insertEnv x (Var fresh) xs1
               bigger2 = insertEnv y (Var fresh) xs2
           in  alphaEquiv' b1 b2 bigger1 bigger2
alphaEquiv' _ _ _ _ = False
