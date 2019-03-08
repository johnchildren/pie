{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Pie.Expr
  ( AtomID(..)
  , VarName(..)
  , Expr(..)
  , ExprF(..)
  )
where

import           Data.Text                                ( Text )
import           Data.Functor.Foldable.TH                 ( makeBaseFunctor )

newtype AtomID = AtomID Text
    deriving (Show, Eq)

newtype VarName = VarName Text
    deriving (Show, Eq)

data Expr = The Expr Expr
         | Var VarName
         | AtomType
         | AtomData AtomID
         | Cons Expr Expr
         | Pair Expr Expr
         | Car Expr
         | Cdr Expr
         | Pie VarName Expr Expr
         | Arrow Expr Expr -- TODO: no arrows in core pie
         | Lambda VarName Expr
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
