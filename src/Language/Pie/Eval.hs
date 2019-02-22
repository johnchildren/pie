module Language.Pie.Eval
  ( TypeError
  , eval
  )
where

import           Data.Functor.Foldable                    ( Fix(..)
                                                          , Base(..)
                                                          , cata
                                                          )
import           Language.Pie.Expr                        ( AtomID(..)
                                                          , VarName(..)
                                                          , Expr(..)
                                                          , ExprF(..)
                                                          )

data TypeError = TypeError
  deriving (Show)

type Algebra t a = Base t a -> a

-- | Evaluate an expression
--
-- Examples:
--
-- >>> let expr = parsePieOrThrow "(car (cons (cons 'aubergine 'courgette) 'tomato))"
-- >>> printPie <$> eval expr
-- Right "(cons 'aubergine 'courgette)"
--
-- >>> let expr = parsePieOrThrow "(Pair (car (cons Atom 'olive)) (cdr (cons 'oil Atom)))"
-- >>> printPie <$> eval expr
-- Right "(Pair Atom Atom)"
eval :: Expr -> Either TypeError Expr
eval = cata eval'

eval' :: Algebra Expr (Either TypeError Expr)
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
eval' (AppF (Right (Lambda v body)) applied) = apply v body applied

apply
  :: VarName -- lambda variable
  -> Expr -- lambda body
  -> Either TypeError Expr -- applied value
  -> Either TypeError Expr
apply _ _                 (Left err) = Left err
apply _ AtomType          _          = Right AtomType
apply _ (AtomData atomID) _          = Right (AtomData atomID)
apply _ Zero              _          = Right Zero
apply v (Cons e1 e2) app = Cons <$> apply v e1 app <*> apply v e2 app
apply v (Pair e1 e2) app = Pair <$> apply v e1 app <*> apply v e2 app
apply v (Car  e1   )      app        = Car <$> apply v e1 app
apply v (Cdr  e1   )      app        = Cdr <$> apply v e1 app
apply v (Add1 e1   )      app        = Add1 <$> apply v e1 app
