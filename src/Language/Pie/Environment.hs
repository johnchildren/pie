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

type Env a = Map.Map VarName a

empty :: Env a
empty = Map.empty

lookup :: VarName -> Env a -> Maybe a
lookup = Map.lookup

insert :: VarName -> a -> Env a -> Env a
insert = Map.insert
