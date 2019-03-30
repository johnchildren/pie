{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Pie.Expr
  ( Expr(..)
  , ExprF(..)
  , Clos(..)
  , CoreExpr(..)
  , CoreExprF(..)
  , toCore
  , fromCore
  )
where

import           Language.Pie.Symbols                     ( Symbol(..)
                                                          , VarName(..)
                                                          )
import           Data.Functor.Foldable                    ( Base
                                                          , cata
                                                          )
import           Data.Functor.Foldable.TH                 ( makeBaseFunctor )

data Expr = The Expr Expr
         | Var VarName
         | Atom
         | Quote Symbol
         | Pair Expr Expr
         | Sigma VarName Expr Expr
         | Cons Expr Expr
         | Car Expr
         | Cdr Expr
         | Arrow Expr Expr
         | Pi VarName Expr Expr
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

-- Closure indiciates this value shouldn't be computed immediately.
newtype Clos = Clos CoreExpr
  deriving (Show, Eq)

-- | Core expression
data CoreExpr = CThe CoreExpr CoreExpr
         | CVar VarName
         | CAtom
         | CQuote Symbol
         | CSigma VarName CoreExpr Clos
         | CCons CoreExpr CoreExpr
         | CCar CoreExpr
         | CCdr CoreExpr
         | CPi VarName CoreExpr Clos
         | CLambda VarName Clos
         | CApp CoreExpr CoreExpr
         | CNat
         | CZero
         | CAdd1 CoreExpr
         | CWhichNat CoreExpr CoreExpr Clos
         | CIterNat CoreExpr CoreExpr Clos
         | CRecNat CoreExpr CoreExpr Clos
         | CUniverse
        deriving (Show, Eq)

makeBaseFunctor ''CoreExpr

type Algebra t a = Base t a -> a

toCore :: Expr -> CoreExpr
toCore = cata toCore'
 where
  toCore' :: Algebra Expr CoreExpr
  toCore' (TheF e1 e2)                 = CThe e1 e2
  toCore' (VarF v    )                 = CVar v
  toCore' AtomF                        = CAtom
  toCore' (QuoteF s      )             = CQuote s
  toCore' (PairF e1 e2   )             = CSigma (VarName "dim") e1 (Clos e2) -- TODO: do something about this varname, otherwise in for a world of pain
  toCore' (SigmaF v e1 e2)             = CSigma v e1 (Clos e2)
  toCore' (ConsF e1 e2   )             = CCons e1 e2
  toCore' (CarF pr       )             = CCar pr
  toCore' (CdrF pr       )             = CCdr pr
  toCore' (ArrowF e1 e2  )             = CPi (VarName "dim") e1 (Clos e2)
  toCore' (PiF v e1 e2   )             = CPi v e1 (Clos e2)
  toCore' (LambdaF v  e  )             = CLambda v (Clos e)
  toCore' (AppF    e1 e2 )             = CApp e1 e2
  toCore' NatF                         = CNat
  toCore' ZeroF                        = CZero
  toCore' (Add1F n                   ) = CAdd1 n
  toCore' (WhichNatF target base step) = CWhichNat target base (Clos step)
  toCore' (IterNatF  target base step) = CIterNat target base (Clos step)
  toCore' (RecNatF   target base step) = CRecNat target base (Clos step)
  toCore' UniverseF                    = CUniverse


fromCore :: CoreExpr -> Expr
fromCore = undefined
