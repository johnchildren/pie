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
import           Language.Pie.Expr                        ( Expr(..) )

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

parseAppExpr :: Parser Expr
parseAppExpr = App <$> pieParser <*> (space1 >> pieParser)

parseTypeVar :: Parser (VarName, Expr)
parseTypeVar = parens $ do
  x <- parseVarName
  space1
  ty <- pieParser
  pure (x, ty)

parsePiExpr :: Parser Expr
parsePiExpr = do
  _ <- string "Pi"
  space1
  (x, ty) <- parseTypeVar
  space1
  Pi x ty <$> pieParser

parseSigmaExpr :: Parser Expr
parseSigmaExpr = do
  _ <- string "Sigma"
  space1
  (x, ty) <- parseTypeVar
  space1
  Sigma x ty <$> pieParser

parsePieExpr :: Parser Expr
parsePieExpr =
  parseBinaryExpr (The <$ string "the")
    <|> parseBinaryExpr (Pair <$ string "Pair")
    <|> parseBinaryExpr (Cons <$ string "cons")
    <|> parseUnaryExpr (Car <$ string "car")
    <|> parseUnaryExpr (Cdr <$ string "cdr")
    <|> parseUnaryExpr (Add1 <$ string "add1")
    <|> parseTernaryExpr (WhichNat <$ string "which-Nat")
    <|> parseTernaryExpr (IterNat <$ string "iter-Nat")
    <|> parseTernaryExpr (RecNat <$ string "rec-Nat")
    <|> parseBinaryExpr (Arrow <$ string "->")
    <|> parseLambdaExpr
    <|> parsePiExpr
    <|> parseSigmaExpr
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
