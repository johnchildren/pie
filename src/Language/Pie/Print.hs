{-# LANGUAGE OverloadedStrings #-}

module Language.Pie.Print
  ( printPie
  )
where

import           Data.Text.Prettyprint.Doc                ( (<>)
                                                          , (<+>)
                                                          , Doc
                                                          , pretty
                                                          )
import           Data.Functor.Foldable                    ( Base
                                                          , cata
                                                          )
import           Language.Pie.Expr                        ( AtomID(..)
                                                          , VarName(..)
                                                          , Expr(..)
                                                          , ExprF(..)
                                                          )

printUnaryExpr :: Doc a -> Doc a -> Doc a
printUnaryExpr tok e1 = "(" <> tok <+> e1 <> ")"

printBinaryExpr :: Doc a -> Doc a -> Doc a -> Doc a
printBinaryExpr tok e1 e2 = "(" <> tok <+> e1 <+> e2 <> ")"

printPie :: Expr -> String
printPie = show . cata printPie'

type Algebra t a = Base t a -> a

printPie' :: Algebra Expr (Doc a)
printPie' (TheF e1 e2      )     = printBinaryExpr "the" e1 e2
printPie' (VarF (VarName v))     = pretty v
printPie' AtomTypeF              = "Atom"
printPie' (AtomDataF (AtomID s)) = "'" <> pretty s
printPie' (PairF e1 e2         ) = printBinaryExpr "Pair" e1 e2
printPie' (ConsF e1 e2         ) = printBinaryExpr "cons" e1 e2
printPie' (CarF e1             ) = printUnaryExpr "car" e1
printPie' (CdrF e1             ) = printUnaryExpr "cdr" e1
printPie' (ArrowF e1 e2        ) = printBinaryExpr "->" e1 e2
printPie' (LambdaF (VarName v) e) =
  printBinaryExpr "lambda" ("(" <> pretty v <> ")") e
printPie' (AppF e1 e2) = "(" <> e1 <+> e2 <> ")"
printPie' ZeroF        = "zero"
printPie' (Add1F e1)   = printUnaryExpr "add1" e1
