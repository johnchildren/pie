module Language.Pie.Eval
  ( TypeError
  , eval
  )
where

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
