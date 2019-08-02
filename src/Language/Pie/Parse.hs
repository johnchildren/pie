module Language.Pie.Parse
  ( parsePie
  , parsePieStatement
  , errorBundlePretty
  , reservedWords
  , Statement(..)
  , PieParseError
  )
where

import           Control.Applicative                      ( pure
                                                          , (<$>)
                                                          , (<$)
                                                          , (<*>)
                                                          , (*>)
                                                          , (<*)
                                                          )
import           Control.Monad                            ( fail
                                                          , (>>=)
                                                          , (>>)
                                                          )
import           Data.Either                              ( Either )
import           Data.Function                            ( flip
                                                          , ($)
                                                          , (.)
                                                          )
import           Data.List                                ( elem )
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Semigroup                           ( (<>) )
import qualified Data.Text                     as Text
import           Data.Text                                ( Text )
import           Data.Tuple                               ( uncurry )
import           Data.Void                                ( Void )
import           Text.Megaparsec                          ( Parsec
                                                          , ParseErrorBundle
                                                          , between
                                                          , eof
                                                          , errorBundlePretty
                                                          , many
                                                          , notFollowedBy
                                                          , parse
                                                          , sepBy1
                                                          , some
                                                          , try
                                                          , (<|>)
                                                          )
import           Text.Megaparsec.Char                     ( alphaNumChar
                                                          , char
                                                          , letterChar
                                                          , numberChar
                                                          , space1
                                                          , string
                                                          )
import           Text.Read                                ( read )
import           Text.Show                                ( show )
import           Language.Pie.Symbols                     ( Symbol(Symbol)
                                                          , VarName(VarName)
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

reservedWords :: [Text] -- list of reserved words
reservedWords =
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

identifier :: Parser Text
identifier = p >>= check
 where
  p :: Parser Text
  p = Text.pack <$> ((:) <$> letterChar <*> many alphaNumChar)

  check :: Text -> Parser Text
  check x = if x `elem` reservedWords
    then fail $ "keyword " <> show x <> " cannot be an identifier"
    else pure x

varNameParser :: Parser VarName
varNameParser = flip VarName 0 <$> identifier

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
    <*> (space1 >> parens (NonEmpty.fromList <$> varNameParser `sepBy1` space1))
    <*> (space1 >> pieParser)

appExprParser :: Parser Expr
appExprParser =
  App
    <$> pieParser
    <*> (space1 >> (NonEmpty.fromList <$> pieParser `sepBy1` space1))

typeVarParser :: Parser (VarName, Expr)
typeVarParser = parens $ do
  x <- varNameParser
  space1
  ty <- pieParser
  pure (x, ty)

piExprParser :: Parser Expr
piExprParser = do
  _ <- rword "Pi"
  space1
  args <- parens $ NonEmpty.fromList <$> typeVarParser `sepBy1` space1
  space1
  Pi args <$> pieParser

sigmaExprParser :: Parser Expr
sigmaExprParser = do
  _ <- rword "Sigma"
  space1
  args <- parens $ NonEmpty.fromList <$> typeVarParser `sepBy1` space1
  space1
  Sigma args <$> pieParser

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
    <|> try piExprParser
    <|> try sigmaExprParser
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
  pure (name, expr)

defineParser :: Parser (VarName, Expr)
defineParser = parens $ do
  _ <- rword "define"
  space1
  name <- varNameParser
  space1
  expr <- pieParser
  pure (name, expr)

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
