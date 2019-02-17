module Main where

import           Text.Parsec

newtype AtomID = AtomID String
    deriving (Show, Eq)

data Pie = Pair Pie Pie
         | Cons Pie Pie
         | AtomType
         | AtomData AtomID
         | Car Pie Pie
         | Cdr Pie Pie
         deriving (Show)

type Parser a = Parsec String () a

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

spaces1 :: Parser ()
spaces1 = skipMany1 space

-- Atom IDs must only contain letters and hyphens
atomID :: Parser AtomID
atomID = do
  char '\''
  id <- many (alphaNum <|> char '-')
  pure $ AtomID id

mkBinaryExprs :: [Parser (Pie -> Pie -> Pie)] -> Parser Pie
mkBinaryExprs = foldr1 (<|>) . fmap (\p -> p <*> (spaces1 >> pie) <*> (spaces1 >> pie))

pieExpr :: Parser Pie
pieExpr = mkBinaryExprs [ Pair <$ string "Pair"
                        , Cons <$ string "cons"
                        , Car <$ string "car"
                        , Cdr <$ string "cdr"
                        ]

-- | Pie type parser
--
-- Examples:
--
-- >>> parse pie "" "Atom"
-- Right AtomType
--
-- >>> parse pie "" "(Pair Atom Atom)"
-- Right (Pair AtomType AtomType)
--
-- >>> parse pie "" "'courgette"
-- Right (AtomData (AtomID "courgette"))
pie :: Parser Pie
pie = (AtomType <$ string "Atom") <|> (AtomData <$> atomID) <|> parens pieExpr

parsePie :: String -> Either ParseError Pie
parsePie = parse pie "<lit>"

parsePieOrThrow :: String -> Pie
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
judgement1 :: Pie -> Pie -> Bool
judgement1 (AtomData _) AtomType = True
judgement1 (Cons d1 d2) (Pair t1 t2) =
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
judgement2 :: Pie -> Pie -> Pie -> Bool
judgement2 (AtomData id1) AtomType (AtomData id2) = id1 == id2
judgement2 (Cons c1 c2) (Pair p1 p2) (Cons c3 c4) =
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
judgement3 :: Pie -> Bool
judgement3 AtomType = True
judgement3 (Pair _ _) = True -- Not strictly true
judgement3 _ = False

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
judgement4 :: Pie -> Pie -> Bool
judgement4 AtomType AtomType = True
judgement4 (Pair p1 p2) (Pair p3 p4) =
  judgement4 p1 p3 && judgement4 p2 p4
judgement4 _ _ = False

data TypeError

normalize :: Pie -> Either TypeError Pie
normalize = undefined

main :: IO ()
main = do
  let foo = parsePieOrThrow "'foo"
  print $ judgement2 foo AtomType foo
