{-# LANGUAGE OverloadedStrings #-}

module Language.Pie.Parse
  ( parsePie
  , parseErrorPretty
  )
where

import           Data.Void                                ( Void )
import qualified Data.Text                     as Text
import           Data.Text                                ( Text )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Language.Pie.Symbols                     ( Symbol(..)
                                                          , VarName(..)
                                                          )
import           Language.Pie.Expr                        ( Expr(..)
                                                          , Clos(..)
                                                          )

type Parser = Parsec Void Text

type PieParseError = ParseError Char Void

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

-- Symbols must only contain letters and hyphens
symbol :: Parser Symbol
symbol = do
  _   <- char '\''
  val <- some (letterChar <|> char '-')
  pure $ Symbol (Text.pack val)

parseVarName :: Parser VarName
parseVarName = VarName . Text.pack <$> some letterChar

parseUnaryExpr :: Parser (Expr -> Expr) -> Parser Expr
parseUnaryExpr p = p <*> (space1 >> pieParser)

parseBinaryExpr :: Parser (Expr -> Expr -> Expr) -> Parser Expr
parseBinaryExpr p = p <*> (space1 >> pieParser) <*> (space1 >> pieParser)

parseLambdaExpr :: Parser Expr
parseLambdaExpr =
  (Lambda <$ string "lambda")
    <*> (space1 >> parens parseVarName)
    <*> (space1 >> (Clos <$> pieParser))

parseAppExpr :: Parser Expr
parseAppExpr = App <$> pieParser <*> (space1 >> pieParser)

parseEliminator :: Parser (Expr -> Expr -> Clos -> Expr) -> Parser Expr
parseEliminator p =
  p
    <*> (space1 >> pieParser)
    <*> (space1 >> pieParser)
    <*> (space1 >> (Clos <$> pieParser))

parsePieExpr :: Parser Expr
parsePieExpr =
  parseBinaryExpr (The <$ string "the")
    <|> parseBinaryExpr (Pair <$ string "Pair")
    <|> parseBinaryExpr (Cons <$ string "cons")
    <|> parseUnaryExpr (Car <$ string "car")
    <|> parseUnaryExpr (Cdr <$ string "cdr")
    <|> parseUnaryExpr (Add1 <$ string "add1")
    <|> parseEliminator (WhichNat <$ string "which-Nat")
    <|> parseEliminator (IterNat <$ string "iter-Nat")
    <|> parseEliminator (RecNat <$ string "rec-Nat")
    <|> parseBinaryExpr (Arrow <$ string "->")
    <|> parseLambdaExpr
    <|> parseAppExpr

pieParser :: Parser Expr
pieParser =
  (Atom <$ string "Atom")
    <|> (Quote <$> symbol)
    <|> (Zero <$ string "zero")
    <|> (Nat <$ string "Nat")
    <|> (Universe <$ string "Universe")
    <|> (Var <$> parseVarName)
    <|> parens parsePieExpr

parsePie :: Text -> Either PieParseError Expr
parsePie = parse (pieParser <* eof) "<lit>"
