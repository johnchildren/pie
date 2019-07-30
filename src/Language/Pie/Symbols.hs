module Language.Pie.Symbols
  ( Symbol(..)
  , VarName(..)
  )
where

import           Prelude                                  ( Show
                                                          , Eq
                                                          , Ord
                                                          , (.)
                                                          )
import           Data.Int                                 ( Int )
import           Data.Text                                ( Text )
import qualified Data.Text                     as Text
import           GHC.Exts                                 ( IsString(fromString)
                                                          )

newtype Symbol = Symbol Text
    deriving (Show, Eq, Ord)

instance IsString Symbol where
  fromString = Symbol . Text.pack

data VarName = VarName Text Int
             | Dimmed Text Int
    deriving (Show, Eq, Ord)
