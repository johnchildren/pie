{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Pie.Expr
  ( Expr(..)
  , ExprF(..)
  , Clos(..)
  )
where

import           Language.Pie.Symbols                     ( Symbol(..)
                                                          , VarName(..)
                                                          )
import           Data.Functor.Foldable.TH                 ( makeBaseFunctor )


-- Closure indiciates this value shouldn't be computed immediately.
newtype Clos = Clos Expr
  deriving (Show, Eq)

data Expr = The Expr Expr
         | Var VarName
         | Atom
         | Quote Symbol
         | Cons Expr Expr
         | Pair Expr Expr
         | Car Expr
         | Cdr Expr
         | Pie VarName Expr Clos
         | Arrow Expr Expr -- TODO: no arrows in core pie
         | Lambda VarName Clos
         | Sigma VarName Expr Clos
         | App Expr Expr
         | Nat
         | Zero
         | Add1 Expr
         | WhichNat Expr Expr Clos
         | IterNat Expr Expr Clos
         | RecNat Expr Expr Clos
         | Universe
        deriving (Show, Eq)

makeBaseFunctor ''Expr
