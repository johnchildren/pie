module Language.Pie.Values
  ( Closure(..)
  , Value(..)
  , Neutral(..)
  , Normal(..)
  , Env
  )
where

import           Language.Pie.Environment                 ( Env )
import           Language.Pie.Symbols                     ( Symbol
                                                          , VarName
                                                          )
import           Language.Pie.Expr                        ( Expr )

data Closure = CLOS (Env Value) VarName Expr
           deriving(Show, Eq)

data Value = PI Value Closure
           | LAM Closure
           | SIGMA Value Closure
           | CONS Value Value
           | PAIR Value Value
           | NAT
           | ZERO
           | ADD1 Value
           | ATOM
           | QUOTE Symbol
           | UNI
           | NEU Value Neutral
           deriving(Show, Eq)

data Neutral = NVar VarName
             | NAp Neutral Normal
             | NCar Neutral
             | NCdr Neutral
             | NWhichNat Neutral Normal Normal
             | NIterNat Neutral Normal Normal
             | NRecNat Neutral Normal Normal
           deriving(Show, Eq)

data Normal = THE Value Value
           deriving(Show, Eq)
