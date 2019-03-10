{-# LANGUAGE NoImplicitPrelude #-}

module Language.Pie.Context
  ( Context
  , empty
  , lookup
  , insert
  )
where

import           Data.Maybe                               ( Maybe )
import qualified Data.Map                      as Map
import           Language.Pie.Symbols                     ( VarName )
import           Language.Pie.Expr                        ( Expr )

type Context = Map.Map VarName Expr

empty :: Context
empty = Map.empty

lookup :: VarName -> Context -> Maybe Expr
lookup = Map.lookup

insert :: VarName -> Expr -> Context -> Context
insert = Map.insert
