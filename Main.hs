{-# LANGUAGE DeriveFunctor #-}

module Main where

import           Control.Category                         ( (>>>) )
import           Text.Parsec

newtype Term f = In { out :: f (Term f) }

type Algebra f a = f a -> a

cata :: (Functor f) => Algebra f a -> Term f -> a
cata f = out >>> fmap (cata f) >>> f

newtype AtomID = AtomID String
    deriving (Show, Eq)

newtype VarName = VarName String
    deriving (Show, Eq)

data Expr f = The f f
         | Var VarName
         | AtomType
         | AtomData AtomID
         | Pair f f
         | Cons f f
         | Car f
         | Cdr f
         | Arrow f f
         | Lambda VarName f
         | App f f
         | Zero
         | Add1 f
         deriving (Show, Functor)

type Parser a = Parsec String () a

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

-- | First form of judgement
-- ______ is a ______.
--
-- Examples:
--
-- >>> let courgette = parsePieOrThrow "'courgette"
-- >>> let atomType = parsePieOrThrow "Atom"
-- >>> judgement1 courgette atomType
-- True
--
-- >>> let consData = parsePieOrThrow "(cons 'courgette 'baguette)"
-- >>> let pairType = parsePieOrThrow "(Pair Atom Atom)"
-- >>> judgement1 consData pairType
-- True
judgement1 :: Term Expr -> Term Expr -> Bool
judgement1 (In (AtomData _)) (In AtomType) = True
judgement1 (In (Cons d1 d2)) (In (Pair t1 t2)) =
  judgement1 d1 t1 && judgement1 d2 t2
judgement1 _ _ = False

-- | Second form of judgement
-- ______ is the same ______ as ______.
--
-- Examples:
--
-- >>> let courgette = parsePieOrThrow "'courgette"
-- >>> let atomType = parsePieOrThrow "Atom"
-- >>> judgement2 courgette atomType courgette
-- True
--
-- >>> let courgette = parsePieOrThrow "'courgette"
-- >>> let atomType = parsePieOrThrow "Atom"
-- >>> let baguette = parsePieOrThrow "'baguette"
-- >>> judgement2 courgette atomType baguette
-- False
judgement2 :: Term Expr -> Term Expr -> Term Expr -> Bool
judgement2 (In (AtomData id1)) (In AtomType) (In (AtomData id2)) = id1 == id2
judgement2 (In (Cons c1 c2)) (In (Pair p1 p2)) (In (Cons c3 c4)) =
  judgement2 c1 p1 c3 && judgement2 c2 p2 c4
judgement2 _ _ _ = False

-- | Third form of judgement
-- _____ is a type.
--
-- Examples:
--
-- >>> let courgette = parsePieOrThrow "'courgette"
-- >>> judgement3 courgette
-- False
--
-- >>> let atomType = parsePieOrThrow "Atom"
-- >>> judgement3 atomType
-- True
judgement3 :: Term Expr -> Bool
judgement3 (In AtomType  ) = True
judgement3 (In (Pair _ _)) = True -- Not strictly true
judgement3 _               = False

-- fourth form of judgement
-- ______ and ______ are the same type.
--
-- Examples:
--
-- >>> let courgette = parsePieOrThrow "'courgette"
-- >>> judgement4 courgette courgette
-- False
--
-- >>> let atomType = parsePieOrThrow "Atom"
-- >>> judgement4 atomType atomType
-- True
judgement4 :: Term Expr -> Term Expr -> Bool
judgement4 (In AtomType) (In AtomType) = True
judgement4 (In (Pair p1 p2)) (In (Pair p3 p4)) =
  judgement4 p1 p3 && judgement4 p2 p4
judgement4 _ _ = False

data TypeError = TypeError
  deriving (Show)

-- | Evaluate an expression
--
-- Examples:
--
-- >>> let expr = parsePieOrThrow "(car (cons (cons 'aubergine 'courgette) 'tomato))"
-- >>> printPie <$> eval expr
-- Right "(cons 'aubergine 'courgette)"
--
-- >>> let expr = parsePieOrThrow "(Pair (car (cons Atom 'olive)) (cdr (cons 'oil Atom)))"
-- >>> printPie <$> eval expr
-- Right "(Pair Atom Atom)"
eval :: Term Expr -> Either TypeError (Term Expr)
eval = cata eval'

eval' :: Algebra Expr (Either TypeError (Term Expr))
eval' AtomType                       = Right (In AtomType)
eval' (AtomData atomID             ) = Right (In (AtomData atomID))
eval' (Cons e1 e2                  ) = (\x y -> In (Cons x y)) <$> e1 <*> e2
eval' (Pair e1 e2                  ) = (\x y -> In (Pair x y)) <$> e1 <*> e2
eval' (Car (Right (In (Cons v1 _)))) = Right v1
eval' (Car (Right (In (Pair v1 _)))) = Right v1
eval' (Car _                       ) = Left TypeError
eval' (Cdr (Right (In (Cons _ v2)))) = Right v2
eval' (Cdr (Right (In (Pair _ v2)))) = Right v2
eval' (Cdr _                       ) = Left TypeError
eval' Zero                           = Right (In Zero)
eval' (Add1 e1)                      = In . Add1 <$> e1

main :: IO ()
main = do
  let foo = parsePieOrThrow "'foo"
  print $ judgement2 foo (In AtomType) foo
