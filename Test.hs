{-# LANGUAGE OverloadedStrings #-}

import qualified Hedgehog                      as Hedge
import qualified Hedgehog.Gen                  as Gen

import           Language.Pie.Expr              ( AtomID(..)
                                                , VarName(..)
                                                , Expr(..)
                                                )
import           Language.Pie.Utils.Recursion   ( Term(..)
                                                , Algebra
                                                , cata
                                                )

genName :: Hedge.MonadGen m => m String
genName = undefined

genPieExpr :: Hedge.MonadGen m => m (Term Expr)
genPieExpr = Gen.recursive
  Gen.choice
  [Hedge.Var <$> genName]
  [ Gen.subtermM genExpr (\x -> Lambda <$> genName <*> pure x)
  , Gen.subterm2 genExpr genExpr App
  ]

prop_parse_print :: Hedge.Property
prop_parse_print = property $ do
  xs <- forAll $ genPieExpr Gen.alpha
  reverse (reverse xs) === xs

tests :: IO Bool
tests =
  checkParallel $ Group "Test.Example" [("prop_parse_print", prop_parse_print)]
