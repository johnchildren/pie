{-# LANGUAGE DeriveFunctor #-}

module Language.Pie.Expr (AtomID(..), VarName(..), Expr(..)) where

newtype AtomID = AtomID String
    deriving (Show, Eq)

newtype VarName = VarName String
    deriving (Show, Eq)

data Expr f = The f f
         | Var VarName
         | AtomType
         | AtomData AtomID
         | Pair f f
         | Cons f f
         | Car f
         | Cdr f
         | Arrow f f
         | Lambda VarName f
         | App f f
         | Zero
         | Add1 f
         deriving (Show, Functor)