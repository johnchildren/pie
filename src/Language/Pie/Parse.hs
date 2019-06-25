module Language.Pie.Parse
  ( parsePie
  , parsePieStatement
  , errorBundlePretty
  , Statement(..)
  , PieParseError
  )
where

import           Prelude
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

type PieParseError = ParseErrorBundle Text Void

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

rword :: Text -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar

-- Symbols must only contain letters and hyphens
symbol :: Parser Symbol
symbol = do
  _   <- char '\''
  val <- some (letterChar <|> char '-')
  pure $ Symbol (Text.pack val)

rws :: [String] -- list of reserved words
rws =
  [ "lambda"
  , "Pi"
  , "Sigma"
  , "the"
  , "Pair"
  , "cons"
  , "car"
  , "cdr"
  , "add1"
  , "which-Nat"
  , "iter-Nat"
  , "rec-Nat"
  , "->"
  , "List"
  , "::"
  , "Atom"
  , "zero"
  , "Nat"
  , "nil"
  , "Universe"
  , "define"
  ]

identifier :: Parser String
identifier = p >>= check
 where
  p = (:) <$> letterChar <*> many alphaNumChar
  check x = if x `elem` rws
    then fail $ "keyword " ++ show x ++ " cannot be an identifier"
    else return x

varNameParser :: Parser VarName
varNameParser = VarName . Text.pack <$> identifier

mkUnaryExprParser :: Parser (Expr -> Expr) -> Parser Expr
mkUnaryExprParser p = p <*> (space1 >> pieParser)

mkBinaryExprParser :: Parser (Expr -> Expr -> Expr) -> Parser Expr
mkBinaryExprParser p = p <*> (space1 >> pieParser) <*> (space1 >> pieParser)

mkTernaryExprParser :: Parser (Expr -> Expr -> Expr -> Expr) -> Parser Expr
mkTernaryExprParser p =
  p
    <*> (space1 >> pieParser)
    <*> (space1 >> pieParser)
    <*> (space1 >> pieParser)

lambdaExprParser :: Parser Expr
lambdaExprParser =
  (Lambda <$ rword "lambda")
    <*> (space1 >> parens varNameParser)
    <*> (space1 >> pieParser)

appExprParser :: Parser Expr
appExprParser = App <$> pieParser <*> (space1 >> pieParser)

typeVarParser :: Parser (VarName, Expr)
typeVarParser = parens $ do
  x <- varNameParser
  space1
  ty <- pieParser
  pure (x, ty)

spacedTypeVarParser :: Parser (VarName, Expr)
spacedTypeVarParser = do
  space1
  pair <- typeVarParser
  space1
  pure pair

piExprParser :: Parser Expr
piExprParser = do
  _       <- rword "Pi"
  (x, ty) <- spacedTypeVarParser
  Pi x ty <$> pieParser

sigmaExprParser :: Parser Expr
sigmaExprParser = do
  _       <- rword "Sigma"
  (x, ty) <- spacedTypeVarParser
  Sigma x ty <$> pieParser

-- Parse an expression in parenthesis (i.e one where application occurs)
parensPieExprParser :: Parser Expr
parensPieExprParser =
  parens
    $   mkBinaryExprParser (The <$ rword "the")
    <|> mkBinaryExprParser (Pair <$ rword "Pair")
    <|> mkBinaryExprParser (Cons <$ rword "cons")
    <|> mkUnaryExprParser (Car <$ rword "car")
    <|> mkUnaryExprParser (Cdr <$ rword "cdr")
    <|> mkUnaryExprParser (Add1 <$ rword "add1")
    <|> mkTernaryExprParser (WhichNat <$ rword "which-Nat")
    <|> mkTernaryExprParser (IterNat <$ rword "iter-Nat")
    <|> mkTernaryExprParser (RecNat <$ rword "rec-Nat")
    <|> mkBinaryExprParser (Arrow <$ rword "->")
    <|> mkUnaryExprParser (List <$ rword "List")
    <|> mkBinaryExprParser (ListExp <$ rword "::")
    <|> lambdaExprParser
    <|> piExprParser
    <|> sigmaExprParser
    -- other application
    <|> appExprParser

pieParser :: Parser Expr
pieParser =
  (Atom <$ rword "Atom")
    <|> (Int . read <$> some numberChar)
    <|> (Quote <$> symbol)
    <|> (Zero <$ rword "zero")
    <|> (Nat <$ rword "Nat")
    <|> (Nil <$ rword "nil")
    <|> (Universe <$ rword "Universe")
    <|> (Var <$> varNameParser)
    <|> parensPieExprParser


claimParser :: Parser (VarName, Expr)
claimParser = parens $ do
  _ <- rword "claim"
  space1
  name <- varNameParser
  space1
  expr <- pieParser
  return (name, expr)

defineParser :: Parser (VarName, Expr)
defineParser = parens $ do
  _ <- rword "define"
  space1
  name <- varNameParser
  space1
  expr <- pieParser
  return (name, expr)

data Statement = Claim VarName Expr
               | Define VarName Expr
               | RawExpr Expr

statementParser :: Parser Statement
statementParser =
  try (uncurry Claim <$> claimParser)
    <|> try (uncurry Define <$> defineParser)
    <|> (RawExpr <$> pieParser)

parsePie :: Text -> Either PieParseError Expr
parsePie = parse (pieParser <* eof) "<lit>"

parsePieStatement :: Text -> Either PieParseError Statement
parsePieStatement = parse (statementParser <* eof) "<stmt>"
