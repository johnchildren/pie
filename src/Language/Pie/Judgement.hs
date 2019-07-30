module Language.Pie.Judgement
  ( judgement1
  , judgement2
  , judgement3
  , judgement4
  , Judgement(..)
  )
where

import           Control.Effect                           ( run )
import           Control.Effect.Fresh                     ( runFresh )
import           Control.Effect.Error                     ( runError )
import           Control.Effect.Reader                    ( runReader )
import           Data.Either                              ( Either(Right, Left)
                                                          )
import           Data.Eq                                  ( Eq((==)) )
import           Data.Function                            ( ($)
                                                          , (.)
                                                          )
import           Text.Show                                ( Show )
import           Language.Pie.TypeChecker                 ( TypeError
                                                            ( UnificationError
                                                            , ReadBackError
                                                            )
                                                          , Binding
                                                          , check
                                                          , convert
                                                          , val
                                                          )
import           Language.Pie.Expr                        ( Expr(..)
                                                          , CoreExpr
                                                          , toCore
                                                          )
import           Language.Pie.Environment                 ( Env )


data Judgement = Yes
    | No
    | TypeError TypeError
    deriving (Show, Eq)


-- | First form of judgement
-- ______ is a ______.
judgement1 :: Env Binding -> Expr -> Expr -> Judgement
judgement1 gamma e1 e2 =
  let checked :: Either TypeError CoreExpr
      checked = run . runError . runFresh . runReader gamma $ do
        t <- val (toCore e2)
        check (toCore e1) t
  in  case checked of
        Right _   -> Yes
        Left  err -> TypeError err

-- | Second form of judgement
-- ______ is the same ______ as ______.
judgement2 :: Env Binding -> Expr -> Expr -> Expr -> Judgement
judgement2 gamma e1 e2 e3 =
  let converted :: Either TypeError (CoreExpr, CoreExpr)
      converted = run . runError . runFresh . runReader gamma $ do
        t  <- val (toCore e2)
        v1 <- val (toCore e1)
        v2 <- val (toCore e3)
        convert t v1 v2
  in  case converted of
        Right (c1, c2)           -> if c1 == c2 then Yes else No
        Left  UnificationError{} -> No
        Left  err                -> TypeError err

-- | Third form of judgement
-- _____ is a type.
judgement3 :: Env Binding -> Expr -> Judgement
judgement3 ctx e = judgement4 ctx e e

-- | Fourth form of judgement
-- ______ and ______ are the same type.
judgement4 :: Env Binding -> Expr -> Expr -> Judgement
judgement4 ctx e1 e2 = case judgement2 ctx e1 Universe e2 of
  TypeError (ReadBackError _ _) -> No
  other                         -> other
