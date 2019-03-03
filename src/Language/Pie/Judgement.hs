module Language.Pie.Judgement
  ( judgement1
  , judgement2
  , judgement3
  , judgement4
  , Judgement(..)
  )
where

import           Language.Pie.Expr                        ( Expr(..) )
import           Language.Pie.Eval                        ( evalPie
                                                          , Env
                                                          , TypeError
                                                          )


data Judgement = Yes
    | No
    | TypeError TypeError
    deriving (Show, Eq)

instance Semigroup Judgement where
  (<>) Yes             Yes             = Yes
  (<>) No              _               = No
  (<>) _               No              = No
  (<>) t@(TypeError _) _               = t
  (<>) _               t@(TypeError _) = t

-- | First form of judgement
-- ______ is a ______.
judgement1 :: Env -> Expr -> Expr -> Judgement
judgement1 _ (AtomData _) AtomType = Yes
judgement1 _ Zero         Nat      = Yes
judgement1 env (Cons d1 d2) (Pair t1 t2) =
  judgement1 env d1 t1 <> judgement1 env d2 t2
judgement1 env e1@(Car _) e2 = case evalPie env e1 of
  Right x   -> judgement1 env x e2
  Left  err -> TypeError err
judgement1 env e1@(Cdr _) e2 = case evalPie env e1 of
  Right x   -> judgement1 env x e2
  Left  err -> TypeError err
judgement1 env e1 e2@(Car _) = case evalPie env e2 of
  Right x   -> judgement1 env e1 x
  Left  err -> TypeError err
judgement1 env e1 e2@(Cdr _) = case evalPie env e2 of
  Right x   -> judgement1 env e1 x
  Left  err -> TypeError err
judgement1 env e1@(App (Lambda _ _) _) e2 = case evalPie env e1 of
  Right x   -> judgement1 env x e2
  Left  err -> TypeError err
judgement1 env e1 e2@(App (Lambda _ _) _) = case evalPie env e2 of
  Right x   -> judgement1 env e1 x
  Left  err -> TypeError err
judgement1 env e1@(Add1 _) Nat = case evalPie env e1 of
  Right _   -> Yes
  Left  err -> TypeError err
judgement1 _ _ _ = No

-- | Second form of judgement
-- ______ is the same ______ as ______.
judgement2 :: Env -> Expr -> Expr -> Expr -> Judgement
judgement2 _ (AtomData id1) AtomType (AtomData id2) =
  if id1 == id2 then Yes else No
judgement2 _ Zero Nat Zero = Yes
judgement2 env (Cons c1 c2) (Pair p1 p2) (Cons c3 c4) =
  judgement2 env c1 p1 c3 <> judgement2 env c2 p2 c4
judgement2 env e1@(Car _) e2 e3 = case evalPie env e1 of
  Right x   -> judgement2 env x e2 e3
  Left  err -> TypeError err
judgement2 env e1@(Cdr _) e2 e3 = case evalPie env e1 of
  Right x   -> judgement2 env x e2 e3
  Left  err -> TypeError err
judgement2 env e1 e2@(Car _) e3 = case evalPie env e2 of
  Right x   -> judgement2 env e1 x e3
  Left  err -> TypeError err
judgement2 env e1 e2@(Cdr _) e3 = case evalPie env e2 of
  Right x   -> judgement2 env e1 x e3
  Left  err -> TypeError err
judgement2 env e1 e2 e3@(Car _) = case evalPie env e3 of
  Right x   -> judgement2 env e1 e2 x
  Left  err -> TypeError err
judgement2 env e1 e2 e3@(Cdr _) = case evalPie env e3 of
  Right x   -> judgement2 env e1 e2 x
  Left  err -> TypeError err
judgement2 env e1@(App (Lambda _ _) _) e2 e3 = case evalPie env e1 of
  Right x   -> judgement2 env x e2 e3
  Left  err -> TypeError err
judgement2 env e1 e2@(App (Lambda _ _) _) e3 = case evalPie env e2 of
  Right x   -> judgement2 env e1 x e3
  Left  err -> TypeError err
judgement2 env e1 e2 e3@(App (Lambda _ _) _) = case evalPie env e3 of
  Right x   -> judgement2 env e1 e2 x
  Left  err -> TypeError err
judgement2 _ _ _ _ = No

-- | Third form of judgement
-- _____ is a type.
judgement3 :: Env -> Expr -> Judgement
judgement3 _   AtomType      = Yes
judgement3 _   (   Pair _ _) = Yes -- Not strictly true
judgement3 env e1@(Car _   ) = case evalPie env e1 of
  Right x   -> judgement3 env x
  Left  err -> TypeError err
judgement3 env e1@(Cdr _) = case evalPie env e1 of
  Right x   -> judgement3 env x
  Left  err -> TypeError err
judgement3 env e1@(App (Lambda _ _) _) = case evalPie env e1 of
  Right x   -> judgement3 env x
  Left  err -> TypeError err
judgement3 _ _ = No

-- | Fourth form of judgement
-- ______ and ______ are the same type.
judgement4 :: Env -> Expr -> Expr -> Judgement
judgement4 _ AtomType AtomType = Yes
judgement4 env (Pair p1 p2) (Pair p3 p4) =
  judgement4 env p1 p3 <> judgement4 env p2 p4
judgement4 env e1@(Car _) e2 = case evalPie env e1 of
  Right x   -> judgement4 env x e2
  Left  err -> TypeError err
judgement4 env e1@(Cdr _) e2 = case evalPie env e1 of
  Right x   -> judgement4 env x e2
  Left  err -> TypeError err
judgement4 env e1 e2@(Car _) = case evalPie env e2 of
  Right x   -> judgement4 env e1 x
  Left  err -> TypeError err
judgement4 env e1 e2@(Cdr _) = case evalPie env e2 of
  Right x   -> judgement4 env e1 x
  Left  err -> TypeError err
judgement4 env e1@(App (Lambda _ _) _) e2 = case evalPie env e1 of
  Right x   -> judgement4 env x e2
  Left  err -> TypeError err
judgement4 env e1 e2@(App (Lambda _ _) _) = case evalPie env e2 of
  Right x   -> judgement4 env e1 x
  Left  err -> TypeError err
judgement4 _ _ _ = No
