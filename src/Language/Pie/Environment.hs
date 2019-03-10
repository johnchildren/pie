{-# LANGUAGE NoImplicitPrelude #-}

module Language.Pie.Environment
  ( Env
  , empty
  , lookup
  , insert
  )
where

import           Data.Maybe                               ( Maybe )
import qualified Data.Map                      as Map
import           Language.Pie.Symbols                     ( VarName )
import           Language.Pie.Values                      ( Value )

type Env = Map.Map VarName Value

empty :: Env
empty = Map.empty

lookup :: VarName -> Env -> Maybe Value
lookup = Map.lookup

insert :: VarName -> Value -> Env -> Env
insert = Map.insert
