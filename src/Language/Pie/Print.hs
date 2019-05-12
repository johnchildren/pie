{-# LANGUAGE OverloadedStrings #-}

module Language.Pie.Print
  ( printPie
  )
where

import           Data.Text.Prettyprint.Doc.Render.Text    ( renderStrict )
import           Data.Text                                ( Text )
import           Data.Text.Prettyprint.Doc                ( (<>)
                                                          , (<+>)
                                                          , Doc
                                                          , pretty
                                                          , layoutPretty
                                                          , defaultLayoutOptions
                                                          )
import           Data.Functor.Foldable                    ( Base
                                                          , cata
                                                          )
import           Language.Pie.Symbols                     ( Symbol(..)
                                                          , VarName(..)
                                                          )
import           Language.Pie.Expr                        ( Expr(..)
                                                          , ExprF(..)
                                                          )

printVarName :: VarName -> Doc a
printVarName (VarName v) = pretty v
printVarName (Dimmed  v) = "_" <> pretty v <> "_"

printUnaryExpr :: Doc a -> Doc a -> Doc a
printUnaryExpr tok e1 = "(" <> tok <+> e1 <> ")"

printBinaryExpr :: Doc a -> Doc a -> Doc a -> Doc a
printBinaryExpr tok e1 e2 = "(" <> tok <+> e1 <+> e2 <> ")"

printTernaryExpr :: Doc a -> Doc a -> Doc a -> Doc a -> Doc a
printTernaryExpr tok e1 e2 e3 = "(" <> tok <+> e1 <+> e2 <+> e3 <> ")"

type Algebra t a = Base t a -> a

printPie :: Expr -> Text
printPie = renderStrict . layoutPretty defaultLayoutOptions . cata printPie'
 where
  printPie' :: Algebra Expr (Doc a)
  printPie' (TheF e1 e2)        = printBinaryExpr "the" e1 e2
  printPie' (VarF v    )        = printVarName v
  printPie' AtomF               = "Atom"
  printPie' (QuoteF (Symbol s)) = "'" <> pretty s
  printPie' (PairF e1 e2      ) = printBinaryExpr "Pair" e1 e2
  printPie' (ConsF e1 e2      ) = printBinaryExpr "cons" e1 e2
  printPie' (CarF e1          ) = printUnaryExpr "car" e1
  printPie' (CdrF e1          ) = printUnaryExpr "cdr" e1
  printPie' (ArrowF e1 e2     ) = printBinaryExpr "->" e1 e2
  printPie' (LambdaF v e) =
    printBinaryExpr "lambda" ("(" <> printVarName v <> ")") e
  printPie' (PiF v e1 e2) =
    printBinaryExpr "Pi" ("(" <> printVarName v <+> e1 <> ")") e2
  printPie' (SigmaF v e1 e2) =
    printBinaryExpr "Sigma" ("(" <> printVarName v <+> e1 <> ")") e2
  printPie' (AppF e1 e2)         = "(" <> e1 <+> e2 <> ")"
  printPie' NatF                 = "Nat"
  printPie' ZeroF                = "zero"
  printPie' (Add1F e1          ) = printUnaryExpr "add1" e1
  printPie' (IntF  n           ) = pretty n
  printPie' (WhichNatF e1 e2 e3) = printTernaryExpr "which-Nat" e1 e2 e3
  printPie' (IterNatF  e1 e2 e3) = printTernaryExpr "iter-Nat" e1 e2 e3
  printPie' (RecNatF   e1 e2 e3) = printTernaryExpr "rec-Nat" e1 e2 e3
  printPie' UniverseF            = "Universe"
