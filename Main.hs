{-# LANGUAGE DeriveFunctor #-}

module Main where

import Language.Pie.Parse (parsePie, parsePieOrThrow)
import Language.Pie.Print (printPie)
import Language.Pie.Eval (eval, TypeError)
import Language.Pie.Expr (AtomID(..), VarName(..), Expr(..))
import Language.Pie.Utils.Recursion (Term(..), Algebra, cata)

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

main :: IO ()
main = do
  let foo = parsePieOrThrow "'foo"
  print $ judgement2 foo (In AtomType) foo
