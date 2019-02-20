module Language.Pie.Parse (parsePie, parsePieOrThrow) where

import Text.Parsec

import Language.Pie.Expr (AtomID(..), VarName(..), Expr(..))
import Language.Pie.Utils.Recursion (Term(..))

type Parser a = Parsec String () a

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

spaces1 :: Parser ()
spaces1 = skipMany1 space

-- Atom IDs must only contain letters and hyphens
atomID :: Parser AtomID
atomID = do
  char '\''
  id <- many1 (alphaNum <|> char '-')
  pure $ AtomID id

parseVarName :: Parser VarName
parseVarName = VarName <$> many1 alphaNum

parseUnaryExpr :: Parser (Term Expr -> Term Expr) -> Parser (Term Expr)
parseUnaryExpr p = p <*> (spaces1 >> pie)

parseBinaryExpr
  :: Parser (Term Expr -> Term Expr -> Term Expr) -> Parser (Term Expr)
parseBinaryExpr p = p <*> (spaces1 >> pie) <*> (spaces1 >> pie)

parseLambdaExpr :: Parser (Term Expr)
parseLambdaExpr =
  ((\x y -> In (Lambda x y)) <$ string "lambda")
    <*> (spaces >> parens parseVarName)
    <*> (spaces >> pie)

parsePieExpr :: Parser (Term Expr)
parsePieExpr =
  parseBinaryExpr ((\x y -> In (Pair x y)) <$ string "Pair")
    <|> (  string "c"
        >> (   parseBinaryExpr ((\x y -> In (Cons x y)) <$ string "ons")
           <|> parseUnaryExpr ((In . Car) <$ string "ar")
           <|> parseUnaryExpr ((In . Cdr) <$ string "dr")
           )
        )
    <|> parseUnaryExpr ((In . Add1) <$ string "add1")
    <|> parseLambdaExpr

-- | Parse a pie expression
--
-- Examples:
--
-- >>> let atomType = parse pie "" "Atom"
-- >>> printPie <$> atomType
-- Right "Atom"
--
-- >>> let pairType = parse pie "" "(Pair Atom Atom)"
-- >>> printPie <$> pairType
-- Right "(Pair Atom Atom)"
--
-- >>> let atom = parse pie "" "'courgette"
-- >>> printPie <$> atom
-- Right "'courgette"
--
-- >>> let lambdaExpr = parse pie "" "(lambda (x) (cons x 'courgette))"
-- >>> printPie <$> lambdaExpr
-- Right "(lambda (x) (cons x 'courgette))"
pie :: Parser (Term Expr)
pie =
  (In AtomType <$ string "Atom")
    <|> (In . AtomData <$> atomID)
    <|> (In Zero <$ string "zero")
    <|> (In . Var <$> parseVarName)
    <|> parens parsePieExpr

parsePie :: String -> Either ParseError (Term Expr)
parsePie = parse pie "<lit>"

parsePieOrThrow :: String -> Term Expr
parsePieOrThrow s = case parsePie s of
  Right pie -> pie
  Left  err -> error (show err)