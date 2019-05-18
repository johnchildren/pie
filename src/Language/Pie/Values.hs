module Language.Pie.Values
  ( Closure(..)
  , Value(..)
  , Neutral(..)
  , Normal(..)
  , Env
  , closName
  )
where

import           Language.Pie.Environment                 ( Env )
import           Language.Pie.Symbols                     ( Symbol
                                                          , VarName
                                                          )
import           Language.Pie.Expr                        ( CoreExpr )

closName :: Closure -> VarName
closName (CLOS _ v _) = v

data Closure = CLOS (Env Value) VarName CoreExpr
           deriving(Show, Eq)

data Value = VPi Value Closure
           | VLambda Closure
           | VSigma Value Closure
           | VCons Value Value
           | VNat
           | VZero
           | VAdd1 Value
           | VAtom
           | VQuote Symbol
           | VUniverse
           | VList Value
           | VNil
           | VListExp Value Value
           | VNeutral Value Neutral
           deriving(Show, Eq)

data Neutral = NVar VarName
             | NAp Neutral Normal
             | NCar Neutral
             | NCdr Neutral
             | NWhichNat Neutral Normal Normal
             | NIterNat Neutral Normal Normal
             | NRecNat Neutral Normal Normal
           deriving(Show, Eq)

data Normal = NormThe Value Value
           deriving(Show, Eq)
