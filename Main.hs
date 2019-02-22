{-# LANGUAGE DeriveFunctor #-}

module Main where

import           Data.Functor.Foldable                    ( Fix(..) )
import           Language.Pie.Parse                       ( parsePie )
import           Language.Pie.Print                       ( printPie )
import           Language.Pie.Eval                        ( evalPie
                                                          , TypeError
                                                          )
import           Language.Pie.Expr                        ( AtomID(..)
                                                          , VarName(..)
                                                          , Expr(..)
                                                          )

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
judgement1 :: Expr -> Expr -> Bool
judgement1 (AtomData _) AtomType     = True
judgement1 (Cons d1 d2) (Pair t1 t2) = judgement1 d1 t1 && judgement1 d2 t2
judgement1 _            _            = False

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
judgement2 :: Expr -> Expr -> Expr -> Bool
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
judgement3 :: Expr -> Bool
judgement3 AtomType   = True
judgement3 (Pair _ _) = True -- Not strictly true
judgement3 _          = False

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
judgement4 :: Expr -> Expr -> Bool
judgement4 AtomType     AtomType     = True
judgement4 (Pair p1 p2) (Pair p3 p4) = judgement4 p1 p3 && judgement4 p2 p4
judgement4 _            _            = False

main :: IO ()
main = do
  let foo = AtomData (AtomID "foo")
  print $ judgement2 foo AtomType foo
