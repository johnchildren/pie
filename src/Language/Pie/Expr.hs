{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Pie.Expr
  ( Expr(..)
  , ExprF(..)
  )
where

import           Language.Pie.Symbols                     ( Symbol(..)
                                                          , VarName(..)
                                                          )
import           Data.Functor.Foldable.TH                 ( makeBaseFunctor )


data Expr = The Expr Expr
         | Var VarName
         | Atom
         | Quote Symbol
         | Cons Expr Expr
         | Pair Expr Expr
         | Car Expr
         | Cdr Expr
         | Pie VarName Expr Expr
         | Arrow Expr Expr -- TODO: no arrows in core pie
         | Lambda VarName Expr
         | Sigma VarName Expr Expr
         | App Expr Expr
         | Nat
         | Zero
         | Add1 Expr
         | WhichNat Expr Expr Expr
         | IterNat Expr Expr Expr
         | RecNat Expr Expr Expr
         | Universe
        deriving (Show, Eq)

makeBaseFunctor ''Expr
