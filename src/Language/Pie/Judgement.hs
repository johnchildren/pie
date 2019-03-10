{-# LANGUAGE ViewPatterns #-}

module Language.Pie.Judgement
  ( judgement1
  , judgement2
  , judgement3
  , judgement4
  , Judgement(..)
  )
where

import           Language.Pie.Expr                        ( Expr(..) )
import           Language.Pie.Environment                 ( Env )
import           Language.Pie.Eval                        ( evalPie
                                                          , TypeError
                                                          )


data Judgement = Yes
    | No
    | TypeError TypeError
    deriving (Show, Eq)

instance Semigroup Judgement where
  (<>) Yes             Yes             = Yes
  (<>) t@(TypeError _) _               = t
  (<>) _               t@(TypeError _) = t
  (<>) No              _               = No
  (<>) _               No              = No


{-
typeOf :: Env -> Expr -> Either TypeError Expr
typeOf _   (Quote _) = Right Atom
typeOf _   Zero         = Right Nat
typeOf _   Atom     = Right Universe
typeOf _   Nat          = Right Universe
typeOf env (Cons e1 e2) = Pair <$> typeOf env e1 <*> typeOf env e2
typeOf _   (Pair _  _ ) = Right Universe
-}

-- | First form of judgement
-- ______ is a ______.
judgement1 :: Env Expr -> Expr -> Expr -> Judgement
judgement1 _ (Quote _) Atom     = Yes
judgement1 _ Zero      Nat      = Yes
judgement1 _ Atom      Universe = Yes
judgement1 _ Nat       Universe = Yes
judgement1 env (Cons d1 d2) (Pair t1 t2) =
  judgement1 env d1 t1 <> judgement1 env d2 t2
judgement1 env e1 e2@(Car _) = case evalPie env e2 of
  Right x   -> judgement1 env e1 x
  Left  err -> TypeError err
judgement1 env e1 e2@(Cdr _) = case evalPie env e2 of
  Right x   -> judgement1 env e1 x
  Left  err -> TypeError err
judgement1 env e1 e2@(App (Lambda _ _) _) = case evalPie env e2 of
  Right x   -> judgement1 env e1 x
  Left  err -> TypeError err
judgement1 env e1@(Add1 _) Nat = case evalPie env e1 of
  Right _   -> Yes
  Left  err -> TypeError err
judgement1 env (evalPie env -> Right x ) e2 = judgement1 env x e2
judgement1 env (evalPie env -> Left err) _  = TypeError err

--judgement1 _   _                         _  = No

-- | Second form of judgement
-- ______ is the same ______ as ______.
judgement2 :: Env Expr -> Expr -> Expr -> Expr -> Judgement
judgement2 _   (Quote id1) Atom (Quote id2) = if id1 == id2 then Yes else No
judgement2 _   Zero        Nat  Zero        = Yes
judgement2 env (Add1 e1)   Nat  (Add1 e2)   = judgement2 env e1 Nat e2
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
judgement3 :: Env Expr -> Expr -> Judgement
judgement3 _   Atom                      = Yes
judgement3 _   (Quote _)                 = No
judgement3 _   Universe                  = Yes
judgement3 _   (Var _                  ) = No -- TODO: check env
judgement3 _   (Cons _  _              ) = No
judgement3 env (Pair e1 e2) = judgement3 env e1 <> judgement3 env e2
judgement3 env (evalPie env -> Right x ) = judgement3 env x
judgement3 env (evalPie env -> Left err) = TypeError err

-- | Fourth form of judgement
-- ______ and ______ are the same type.
judgement4 :: Env Expr -> Expr -> Expr -> Judgement
judgement4 _ Atom     Atom     = Yes
judgement4 _ Universe Universe = Yes
judgement4 env (Pair e1 e2) (Pair e3 e4) =
  judgement4 env e1 e3 <> judgement4 env e2 e4
judgement4 env (Arrow e1 e2) (Arrow e3 e4) =
  judgement4 env e1 e3 <> judgement4 env e2 e4
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
