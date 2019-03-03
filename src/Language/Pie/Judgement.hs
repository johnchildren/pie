module Language.Pie.Judgement
  ( judgement1
  , judgement2
  , judgement3
  , judgement4
  )
where

import           Language.Pie.Expr                        ( Expr(..) )

-- | First form of judgement
-- ______ is a ______.
judgement1 :: Expr -> Expr -> Bool
judgement1 (AtomData _) AtomType     = True
judgement1 (Cons d1 d2) (Pair t1 t2) = judgement1 d1 t1 && judgement1 d2 t2
judgement1 _            _            = False

-- | Second form of judgement
-- ______ is the same ______ as ______.
judgement2 :: Expr -> Expr -> Expr -> Bool
judgement2 (AtomData id1) AtomType (AtomData id2) = id1 == id2
judgement2 (Cons c1 c2) (Pair p1 p2) (Cons c3 c4) =
  judgement2 c1 p1 c3 && judgement2 c2 p2 c4
judgement2 _ _ _ = False

-- | Third form of judgement
-- _____ is a type.
judgement3 :: Expr -> Bool
judgement3 AtomType   = True
judgement3 (Pair _ _) = True -- Not strictly true
judgement3 _          = False

-- | Fourth form of judgement
-- ______ and ______ are the same type.
judgement4 :: Expr -> Expr -> Bool
judgement4 AtomType     AtomType     = True
judgement4 (Pair p1 p2) (Pair p3 p4) = judgement4 p1 p3 && judgement4 p2 p4
judgement4 _            _            = False
