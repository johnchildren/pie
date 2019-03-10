module Language.Pie.Symbols
  ( Symbol(..)
  , VarName(..)
  )
where

import           Data.Text                                ( Text )

newtype Symbol = Symbol Text
    deriving (Show, Eq)

newtype VarName = VarName Text
    deriving (Show, Eq, Ord)
