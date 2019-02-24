module Language.Pie.Parse
  ( parsePie
  )
where

import           Text.Parsec

import           Language.Pie.Expr                        ( AtomID(..)
                                                          , VarName(..)
                                                          , Expr(..)
                                                          )

type Parser a = Parsec String () a

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

spaces1 :: Parser ()
spaces1 = skipMany1 space

-- Atom IDs must only contain letters and hyphens
atomID :: Parser AtomID
atomID = do
  _ <- char '\''
  val <- many1 (alphaNum <|> char '-')
  pure $ AtomID val

parseVarName :: Parser VarName
parseVarName = VarName <$> many1 alphaNum

parseUnaryExpr :: Parser (Expr -> Expr) -> Parser Expr
parseUnaryExpr p = p <*> (spaces1 >> pieParser)

parseBinaryExpr :: Parser (Expr -> Expr -> Expr) -> Parser Expr
parseBinaryExpr p = p <*> (spaces1 >> pieParser) <*> (spaces1 >> pieParser)

parseLambdaExpr :: Parser Expr
parseLambdaExpr =
  (Lambda <$ string "lambda")
    <*> (spaces >> parens parseVarName)
    <*> (spaces >> pieParser)

parsePieExpr :: Parser Expr
parsePieExpr =
  parseBinaryExpr (Pair <$ string "Pair")
    <|> (  string "c"
        >> (   parseBinaryExpr (Cons <$ string "ons")
           <|> parseUnaryExpr (Car <$ string "ar")
           <|> parseUnaryExpr (Cdr <$ string "dr")
           )
        )
    <|> parseUnaryExpr (Add1 <$ string "add1")
    <|> parseLambdaExpr

pieParser :: Parser Expr
pieParser =
  (AtomType <$ string "Atom")
    <|> (AtomData <$> atomID)
    <|> (Zero <$ string "zero")
    <|> (Var <$> parseVarName)
    <|> parens parsePieExpr

parsePie :: String -> Either ParseError Expr
parsePie = parse pieParser "<lit>"
