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

data Value = PI Value Closure
           | LAM Closure
           | SIGMA Value Closure
           | PAIR Value Value
           | NAT
           | ZERO
           | ADD1 Value
           | EQ Value Value Value
           | SAME
           | TRIVIAL
           | SOLE
           | ABSURD
           | ATOM
           | QUOTE Symbol
           | UNI
           | NEU Value Neutral

data Neutral = NVar VarName
             | NAp Neutral Normal
             | NCar Neutral
             | NCdr Neutral
             | NIndNat Neutral Neutral Neutral Neutral
             | NReplace Neutral Normal Normal
             | NIndAbsurd Neutral Normal

data Normal = THE Value Value
