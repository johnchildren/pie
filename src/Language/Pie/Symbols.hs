module Language.Pie.Symbols
  ( Symbol(..)
  , VarName(..)
  )
where

import           Data.Text                                ( Text )
import qualified Data.Text                     as Text
import           GHC.Exts                                 ( IsString(..) )

newtype Symbol = Symbol Text
    deriving (Show, Eq, Ord)

instance IsString Symbol where
  fromString = Symbol . Text.pack

data VarName = VarName Text
             | Dimmed Text
    deriving (Show, Eq, Ord)
