{-# LANGUAGE OverloadedStrings #-}

import           Hedgehog
import qualified Hedgehog.Gen                  as Gen

genName :: MonadGen m => m String

genPieExpr :: MonadGen m => m Expr
genPieExpr = Gen.recursive
  Gen.choice
  [Var <$> genName]
  [ Gen.subtermM genExpr (x -> Lam <$> genName <*> pure x)
  , Gen.subterm2 genExpr genExpr App
  ]

prop_parse_print :: Property
prop_parse_print = property $ do
  xs <- forAll $ genPieExpr Gen.alpha
  reverse (reverse xs) === xs

tests :: IO Bool
tests =
  checkParallel $ Group "Test.Example" [("prop_parse_print", prop_parse_print)]
