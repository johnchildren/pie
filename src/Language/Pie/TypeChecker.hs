{-# LANGUAGE ViewPatterns #-}

module TypeChecker
  ( alphaEquiv
  )
where


import           Language.Pie.Symbols                     ( VarName )
import           Language.Pie.Environment                 ( Env )
import qualified Language.Pie.Environment      as Env
import           Language.Pie.Values                      ( Value(..)
                                                          , Closure(..)
                                                          )
import           Language.Pie.Expr                        ( Expr(..) )


doCar :: Value -> Value
doCar = undefined

doCdr :: Value -> Value
doCdr = undefined

doApp :: Value -> Value -> Value
doApp = undefined

val :: Env Value -> Expr -> Value
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
  val' Atom             = ATOM
  val' (Quote ad      ) = QUOTE ad
  val' (App rator rand) = doApp (val env rator) (val env rand)
  -- ind-Nat
  --val' (Var x) = Env.lookup env x

-- Expressions that have no arguments to their constructor
-- TODO: I'm sure there's a better word for this?
isAtomic :: Expr -> Bool
isAtomic Atom     = True
isAtomic Nat      = True
isAtomic Zero     = True
isAtomic Universe = True
isAtomic _        = False

type Bindings = Env VarName

-- generate a symbol not in either environment
freshen :: Bindings -> Bindings -> VarName
freshen = undefined

alphaEquiv :: Expr -> Expr -> Bool
alphaEquiv e1 e2 = alphaEquiv' e1 e2 Env.empty Env.empty

alphaEquiv' :: Expr -> Expr -> Bindings -> Bindings -> Bool
alphaEquiv' e1 e2 _ _ | e1 == e2 && isAtomic e1 = True
alphaEquiv' (Var v1) (Var v2) (Env.lookup v1 -> Nothing) (Env.lookup v2 -> Nothing)
  = v1 == v2
alphaEquiv' (Var v1) (Var v2) (Env.lookup v1 -> Just e3) (Env.lookup v2 -> Just e4)
  = e3 == e4
alphaEquiv' (Var   _ ) (Var   _ ) _ _ = False
alphaEquiv' (Quote a1) (Quote a2) _ _ = a1 == a2
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
