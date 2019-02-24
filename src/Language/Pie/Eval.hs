module Language.Pie.Eval
  ( TypeError
  , evalPie
  )
where

import           Data.Functor.Foldable                    ( Base
                                                          , cata
                                                          )
import           Language.Pie.Expr                        ( VarName(..)
                                                          , Expr(..)
                                                          , ExprF(..)
                                                          )

data TypeError = TypeError
  deriving (Show, Eq)

type Algebra t a = Base t a -> a

evalPie :: Expr -> Either TypeError Expr
evalPie = cata eval'

eval' :: Algebra Expr (Either TypeError Expr)
eval' (TheF e1 e2  )             = The <$> e1 <*> e2 --TODO: this should be a type check
eval' (VarF varname)             = Right (Var varname)
eval' AtomTypeF                  = Right AtomType
eval' (AtomDataF atomID        ) = Right (AtomData atomID)
eval' (ConsF e1 e2             ) = Cons <$> e1 <*> e2
eval' (PairF e1 e2             ) = Pair <$> e1 <*> e2
eval' (CarF (Right (Cons v1 _))) = Right v1
eval' (CarF (Right (Pair v1 _))) = Right v1
eval' (CarF _                  ) = Left TypeError
eval' (CdrF (Right (Cons _ v2))) = Right v2
eval' (CdrF (Right (Pair _ v2))) = Right v2
eval' (CdrF _                  ) = Left TypeError
eval' ZeroF                      = Right Zero
eval' (Add1F e1)                 = Add1 <$> e1
eval' (ArrowF e1 e2)             = Arrow <$> e1 <*> e2
eval' (LambdaF var expr)         = Lambda var <$> expr
eval' (AppF (Right (Lambda v body)) applied) = apply v body <$> applied
eval' (AppF _ _)                 = Left TypeError -- TODO: is this true?

apply
  :: VarName -- lambda variable
  -> Expr -- lambda body
  -> Expr -- applied value
  -> Expr
apply v (The e1 e2  )     app = The (apply v e1 app) (apply v e2 app)
apply v (Var varname)     app = if v == varname then app else Var varname
apply _ AtomType          _   = AtomType
apply _ (AtomData atomID) _   = AtomData atomID
apply _ Zero              _   = Zero
apply v (Cons e1 e2     ) app = Cons (apply v e1 app) (apply v e2 app)
apply v (Pair e1 e2     ) app = Pair (apply v e1 app) (apply v e2 app)
apply v (Car  e1        ) app = Car (apply v e1 app)
apply v (Cdr  e1        ) app = Cdr (apply v e1 app)
apply v (Add1 e1        ) app = Add1 (apply v e1 app)
apply v (Arrow  e1  e2  ) app = Arrow (apply v e1 app) (apply v e2 app)
apply v (Lambda var expr) app = Lambda var (apply v expr app)
apply v (App    e1  e2  ) app = App (apply v e1 app) (apply v e2 app) -- TODO: don't think this should ever happen?
