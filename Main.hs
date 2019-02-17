module Main where

import Text.Parsec

newtype AtomID = AtomID String
    deriving (Show, Eq)

data Pie = Type PieType
         | Data PieData
         deriving (Show)

data PieType = PairType PieType PieType
             | AtomType
             deriving (Show)

data PieData = ConsData PieData PieData
             | AtomData AtomID
             deriving (Show)

mkAtom :: String -> Pie
mkAtom s = Data (AtomData (AtomID s))

type Parser a = Parsec String () a

-- | Pie type parser
--
-- Examples:
--
-- >>> parse pieType "" "Atom"
-- Right AtomType
pieType :: Parser PieType
pieType = (AtomType <$ string "Atom")

-- Atom IDs must only contain letters and hyphens
atomID :: Parser AtomID
atomID = do
    string "'"
    id <- many (alphaNum <|> char '-')
    pure $ AtomID id

-- | Pie data parser
-- >>> parse pieData "" "'courgette"
-- Right (AtomData (AtomID "courgette"))
pieData :: Parser PieData
pieData = (AtomData <$> atomID)

pie :: Parser Pie
pie = (Type <$> pieType) <|> (Data <$> pieData)

parsePie :: String -> Either ParseError Pie
parsePie = parse pie "<lit>"

parsePieOrThrow :: String -> Pie
parsePieOrThrow s = case parsePie s of
    Right pie -> pie
    Left err -> error (show err)

-- | First form of judgement
-- ______ is a ______.
--
-- Examples:
--
-- >>> let courgette = parsePieOrThrow "'courgette"
-- >>> let atomType = parsePieOrThrow "Atom"
-- >>> judgement1 courgette atomType
-- True
judgement1 :: Pie -> Pie -> Bool
judgement1 (Data d) (Type t) = judgement1' d t
judgement1 _ _ = False

-- After pre-supposition
judgement1' :: PieData -> PieType -> Bool
judgement1' (AtomData _) AtomType = True
judgement1' (ConsData d1 d2) (PairType t1 t2) = judgement1' d1 t1 && judgement1' d2 t2
judgement1' _ _ = False

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
judgement2 (Data d1) (Type t) (Data d2) = judgement2' d1 t d2
judgement2 _ _ _ = False

-- After pre-supposition
judgement2' :: PieData -> PieType -> PieData -> Bool
judgement2' (AtomData id1) AtomType (AtomData id2) = id1 == id2
judgement2' (ConsData c1 c2) (PairType p1 p2) (ConsData c3 c4) = judgement2' c1 p1 c3 && judgement2' c2 p2 c4
judgement2' _ _ _ = False

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
judgement3 (Type _) = True
judgement3 (Data _) = False

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
judgement4 (Type t1) (Type t2) = judgement4' t1 t2
judgement4 _ _ = False

-- after pre-supposition
judgement4' :: PieType -> PieType -> Bool
judgement4' AtomType AtomType = True
judgement4' (PairType p1 p2) (PairType p3 p4) = judgement4' p1 p3 && judgement4' p2 p4
judgement4' _ _ = False

data TypeError

normalize :: Pie -> Either TypeError Pie
normalize = undefined

main :: IO ()
main = do
    let foo = mkAtom "foo"
    print $ judgement2 foo (Type AtomType) foo
