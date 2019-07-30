module Language.Pie.Print
  ( printPie
  )
where

import           Control.Applicative                      ( (<$>) )
import           Data.Function                            ( (.)
                                                          , ($)
                                                          )
import           Data.Eq                                  ( (==) )
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Text.Prettyprint.Doc.Render.Text    ( renderStrict )
import           Data.Text                                ( Text )
import           Data.Text.Prettyprint.Doc                ( (<>)
                                                          , (<+>)
                                                          , Doc
                                                          , pretty
                                                          , enclose
                                                          , encloseSep
                                                          , hsep
                                                          , lparen
                                                          , rparen
                                                          , space
                                                          , layoutPretty
                                                          , defaultLayoutOptions
                                                          )
import           Data.Functor.Foldable                    ( Base
                                                          , cata
                                                          )
import           Language.Pie.Symbols                     ( Symbol(Symbol)
                                                          , VarName
                                                            ( VarName
                                                            , Dimmed
                                                            )
                                                          )
import           Language.Pie.Expr                        ( Expr
                                                          , ExprF(..)
                                                          )

-- for sigma and pi
typePair :: (VarName, Doc a) -> Doc a
typePair (x, y) = enclose lparen rparen (printVarName x <+> y)

printVarName :: VarName -> Doc a
printVarName (VarName v n) = pretty v <> if n == 0 then "" else pretty n
printVarName (Dimmed  v _) = "_" <> pretty v <> "_"

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
  printPie' (ArrowF  e1 e2    ) = printBinaryExpr "->" e1 e2
  printPie' (LambdaF vs e     ) = printBinaryExpr
    "lambda"
    (encloseSep lparen rparen space $ printVarName <$> NonEmpty.toList vs)
    e
  printPie' (PiF vs e2) =
    printBinaryExpr "Pi" (hsep (typePair <$> NonEmpty.toList vs)) e2
  printPie' (SigmaF vs e2) =
    printBinaryExpr "Sigma" (hsep (typePair <$> NonEmpty.toList vs)) e2
  printPie' (AppF e es) =
    encloseSep lparen rparen space (e : NonEmpty.toList es)
  printPie' NatF                 = "Nat"
  printPie' ZeroF                = "zero"
  printPie' (Add1F e1          ) = printUnaryExpr "add1" e1
  printPie' (IntF  n           ) = pretty n
  printPie' (WhichNatF e1 e2 e3) = printTernaryExpr "which-Nat" e1 e2 e3
  printPie' (IterNatF  e1 e2 e3) = printTernaryExpr "iter-Nat" e1 e2 e3
  printPie' (RecNatF   e1 e2 e3) = printTernaryExpr "rec-Nat" e1 e2 e3
  printPie' (ListF e           ) = printUnaryExpr "List" e
  printPie' NilF                 = "nil"
  printPie' (ListExpF e1 e2)     = printBinaryExpr "::" e1 e2
  printPie' UniverseF            = "Universe"
