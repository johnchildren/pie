{-# LANGUAGE OverloadedStrings #-}

module Language.Pie.Parse
  ( parsePie
  )
where

import           Data.Void                                ( Void )
import           Data.Text                                ( Text )
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Language.Pie.Expr                        ( AtomID(..)
                                                          , VarName(..)
                                                          , Expr(..)
                                                          )

type Parser = Parsec Void Text

type PieParseError = ParseError Char Void

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

-- Atom IDs must only contain letters and hyphens
atomID :: Parser AtomID
atomID = do
  _   <- char '\''
  val <- some (letterChar <|> char '-')
  pure $ AtomID val

parseVarName :: Parser VarName
parseVarName = VarName <$> some letterChar

parseUnaryExpr :: Parser (Expr -> Expr) -> Parser Expr
parseUnaryExpr p = p <*> (space1 >> pieParser)

parseBinaryExpr :: Parser (Expr -> Expr -> Expr) -> Parser Expr
parseBinaryExpr p = p <*> (space1 >> pieParser) <*> (space1 >> pieParser)

parseTernaryExpr :: Parser (Expr -> Expr -> Expr -> Expr) -> Parser Expr
parseTernaryExpr p =
  p
    <*> (space1 >> pieParser)
    <*> (space1 >> pieParser)
    <*> (space1 >> pieParser)

parseLambdaExpr :: Parser Expr
parseLambdaExpr =
  (Lambda <$ string "lambda")
    <*> (space1 >> parens parseVarName)
    <*> (space1 >> pieParser)

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
    <|> parseTernaryExpr (WhichNat <$ string "which-Nat")
    <|> parseTernaryExpr (IterNat <$ string "iter-Nat")
    <|> parseTernaryExpr (IterNat <$ string "rec-Nat")
    <|> parseLambdaExpr

pieParser :: Parser Expr
pieParser =
  (AtomType <$ string "Atom")
    <|> (AtomData <$> atomID)
    <|> (Zero <$ string "zero")
    <|> (Nat <$ string "Nat")
    <|> (Var <$> parseVarName)
    <|> parens parsePieExpr

parsePie :: Text -> Either PieParseError Expr
parsePie = parse pieParser "<lit>"
