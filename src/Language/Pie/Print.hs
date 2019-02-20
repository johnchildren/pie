module Language.Pie.Print
  ( printPie
  )
where

printUnaryExpr :: String -> String -> String
printUnaryExpr tok e1 = "(" ++ tok ++ " " ++ e1 ++ ")"

printBinaryExpr :: String -> String -> String -> String
printBinaryExpr tok e1 e2 = "(" ++ tok ++ " " ++ e1 ++ " " ++ e2 ++ ")"

-- | Print an expression
--
-- Examples:
--
-- >>> let consData = parsePieOrThrow "(cons 'courgette 'baguette)"
-- >>> printPie consData
-- "(cons 'courgette 'baguette)"
--
-- >>> let pairType = parsePieOrThrow "(Pair Atom Atom)"
-- >>> printPie pairType
-- "(Pair Atom Atom)"
--
-- >>> let four = parsePieOrThrow "(add1 (add1 (add1 (add1 zero))))"
-- >>> printPie four
-- "(add1 (add1 (add1 (add1 zero))))"
printPie :: Term Expr -> String
printPie = cata printPie'

printPie' :: Algebra Expr String
printPie' (The e1 e2      )       = printBinaryExpr "the" e1 e2
printPie' (Var (VarName v))       = v
printPie' AtomType                = "Atom"
printPie' (AtomData (AtomID s)  ) = "'" ++ s
printPie' (Pair e1 e2           ) = printBinaryExpr "Pair" e1 e2
printPie' (Cons e1 e2           ) = printBinaryExpr "cons" e1 e2
printPie' (Car e1               ) = printUnaryExpr "car" e1
printPie' (Cdr e1               ) = printUnaryExpr "cdr" e1
printPie' (Arrow  e1          e2) = printBinaryExpr "->" e1 e2
printPie' (Lambda (VarName v) e ) = printBinaryExpr "lambda" ("(" ++ v ++ ")") e
printPie' (App    e1          e2) = "(" ++ e1 ++ " " ++ e2 ++ ")"
printPie' Zero                    = "zero"
printPie' (Add1 e1)               = printUnaryExpr "add1" e1
