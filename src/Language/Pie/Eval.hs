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
 where
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

apply :: VarName -> Expr -> Expr -> Expr
apply v app = cata apply'
 where
  apply' :: Algebra Expr Expr
  apply' (TheF e1 e2  )     = The e1 e2
  apply' (VarF varname)     = if v == varname then app else Var varname
  apply' AtomTypeF          = AtomType
  apply' (AtomDataF atomID) = AtomData atomID
  apply' ZeroF              = Zero
  apply' (ConsF e1 e2     ) = Cons e1 e2
  apply' (PairF e1 e2     ) = Pair e1 e2
  apply' (CarF  e1        ) = Car e1
  apply' (CdrF  e1        ) = Cdr e1
  apply' (Add1F e1        ) = Add1 e1
  apply' (ArrowF  e1  e2  ) = Arrow e1 e2
  apply' (LambdaF var expr) = Lambda var expr
  apply' (AppF    e1  e2  ) = App e1 e2 -- TODO: don't think this should ever happen?
