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

import           Data.Functor.Foldable.TH                 ( makeBaseFunctor )

newtype AtomID = AtomID String
    deriving (Show, Eq)

newtype VarName = VarName String
    deriving (Show, Eq)

data Expr = The Expr Expr
         | Var VarName
         | AtomType
         | AtomData AtomID
         | Pair Expr Expr
         | Cons Expr Expr
         | Car Expr
         | Cdr Expr
         | Arrow Expr Expr
         | Lambda VarName Expr
         | App Expr Expr
         | Nat
         | Zero
         | Add1 Expr
         | WhichNat Expr Expr Expr
         | IterNat Expr Expr Expr
         | RecNat Expr Expr Expr
        deriving (Show, Eq)

makeBaseFunctor ''Expr
