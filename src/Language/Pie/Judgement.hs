module Language.Pie.Judgement
  ( judgement1
  , judgement2
  , judgement3
  , judgement4
  , Judgement(..)
  )
where

import           Language.Pie.TypeChecker                 ( TypeError
                                                          , Binding
                                                          , check
                                                          , convert
                                                          , val
                                                          , ctxToEnvironment
                                                          )
import           Language.Pie.Expr                        ( Expr(..) )
import           Language.Pie.Environment                 ( Env )


data Judgement = Yes
    | No
    | TypeError TypeError
    deriving (Show, Eq)


-- | First form of judgement
-- ______ is a ______.
judgement1 :: Env Binding -> Expr -> Expr -> Judgement
judgement1 ctx e1 e2 =
  let env = ctxToEnvironment ctx
      t   = val env e2
  in  case t >>= check ctx e1 of
        Right _   -> Yes
        Left  err -> TypeError err

-- | Second form of judgement
-- ______ is the same ______ as ______.
judgement2 :: Env Binding -> Expr -> Expr -> Expr -> Judgement
judgement2 ctx e1 e2 e3 =
  let env = ctxToEnvironment ctx
      converted :: Either TypeError (Expr, Expr)
      converted = do
        t  <- val env e2
        v1 <- val env e1
        v2 <- val env e3
        convert ctx t v1 v2
  in  case converted of
        Right (c1, c2) -> if c1 == c2 then Yes else No
        Left  err      -> TypeError err

-- | Third form of judgement
-- _____ is a type.
judgement3 :: Env Binding -> Expr -> Judgement
judgement3 ctx e = judgement4 ctx e e

-- | Fourth form of judgement
-- ______ and ______ are the same type.
judgement4 :: Env Binding -> Expr -> Expr -> Judgement
judgement4 ctx e1 = judgement2 ctx e1 Universe
