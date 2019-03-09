module Language.Pie.Environment
  ( Env
  , emptyEnv
  )
where

import qualified Data.Map                      as Map
import           Language.Pie.Expr                        ( Expr
                                                          , VarName
                                                          )

type Env = Map.Map VarName Expr

emptyEnv :: Env
emptyEnv = Map.empty
