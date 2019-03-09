module Language.Pie.Eval
  ( TypeError
  , evalPie
  )
where

import           Data.Functor.Foldable                    ( Base
                                                          , cata
                                                          )
import           Language.Pie.Environment                 ( Env )
import           Language.Pie.Expr                        ( VarName
                                                          , Expr(..)
                                                          , ExprF(..)
                                                          )

-- worst type errors ever
newtype TypeError = TypeError String
  deriving (Show, Eq)

type Algebra t a = Base t a -> a

evalPie :: Env -> Expr -> Either TypeError Expr
evalPie env = cata eval'
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
  eval' (CarF _                  ) = Left (TypeError "can't car")
  eval' (CdrF (Right (Cons _ v2))) = Right v2
  eval' (CdrF (Right (Pair _ v2))) = Right v2
  eval' (CdrF _                  ) = Left (TypeError "can't cdr")
  eval' NatF                       = Right Nat
  eval' ZeroF                      = Right Zero
  eval' (Add1F e1@(Right Zero))    = Add1 <$> e1
  eval' (Add1F e1@(Right (Add1 _))) = Add1 <$> e1
  eval' (Add1F e1@(Right (Var _))) = Add1 <$> e1
  eval' (Add1F _)                  = Left (TypeError "can't add")

  -- functions
  eval' (PieF var e1 e2)           = Pie var <$> e1 <*> e2
  eval' (ArrowF e1 e2)             = Arrow <$> e1 <*> e2
  eval' (LambdaF var expr)         = Lambda var <$> expr
  eval' (AppF (Right (Lambda v body)) applied) = apply v body <$> applied
  eval' (AppF _ _)                 = Left (TypeError "can't apply") -- TODO: is this true?
  -- which-Nat
  eval' (WhichNatF (Right Zero) base _) = base
  eval' (WhichNatF (Right (Add1 n)) _ step) =
    flip App n <$> step >>= evalPie env
  eval' WhichNatF{}                    = Left (TypeError "can't which-Nat")
  -- iter-Nat
  eval' (IterNatF (Right Zero) base _) = base
  eval' (IterNatF (Right (Add1 n)) base step) =
    ((\b s -> App s (IterNat n b s)) <$> base <*> step) >>= evalPie env
  eval' IterNatF{}                    = Left (TypeError "can't iter-Nat")
  -- rec-Nat
  eval' (RecNatF (Right Zero) base _) = base
  eval' (RecNatF (Right (Add1 n)) base step) =
    ((\b s -> App (App s n) (RecNat n b s)) <$> base <*> step) >>= evalPie env
  eval' RecNatF{} = Left (TypeError "can't rec-Nat")
  eval' UniverseF = Right Universe

apply :: VarName -> Expr -> Expr -> Expr
apply v body applied = cata apply' body
 where
  apply' :: Algebra Expr Expr
  apply' (TheF e1 e2  )       = The e1 e2
  apply' (VarF varname)       = if v == varname then applied else Var varname
  apply' AtomTypeF            = AtomType
  apply' (AtomDataF atomID)   = AtomData atomID
  apply' ZeroF                = Zero
  apply' NatF                 = Nat
  apply' (ConsF e1 e2       ) = Cons e1 e2
  apply' (PairF e1 e2       ) = Pair e1 e2
  apply' (CarF  e1          ) = Car e1
  apply' (CdrF  e1          ) = Cdr e1
  apply' (Add1F e1          ) = Add1 e1
  apply' (PieF var e1 e2    ) = Pie var e1 e2
  apply' (ArrowF  e1  e2    ) = Arrow e1 e2
  apply' (LambdaF var expr  ) = Lambda var expr
  apply' (AppF    e1  e2    ) = App e1 e2 -- TODO: don't think this should ever happen?
  apply' (WhichNatF e1 e2 e3) = WhichNat e1 e2 e3
  apply' (IterNatF  e1 e2 e3) = IterNat e1 e2 e3
  apply' (RecNatF   e1 e2 e3) = RecNat e1 e2 e3
  apply' UniverseF            = Universe
