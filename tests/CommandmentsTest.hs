{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module CommandmentsTest
  ( test_commandments
  )
where

import           Prelude
import           Control.Effect
import           Control.Effect.Error                     ( runError )
import           Control.Effect.State                     ( evalState )
import           Data.Text                                ( Text )
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Language.Pie.Interpreter                 ( interp
                                                          , InterpError
                                                          , IState
                                                          )
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import qualified Language.Pie.Environment      as Env

runInterp :: Text -> Either InterpError Text
runInterp = run . runError . evalState @IState (Env.empty, Env.empty) . interp

test_commandments :: TestTree
test_commandments = testGroup
  "Commandments related to Pie"
  [ testGroup
    "The Law of Tick Marks"
    [ testProperty
        "Two expressions are the same Atom if their values are tick marks followed by identical letters and hyphens."
      $ property
      $ do
          value <- forAll $ Gen.text (Range.constant 1 10) Gen.alpha
          runInterp ("(= Atom " <> "'" <> value <> " '" <> value <> ")")
            === Right "(the Universe (Atom 'foo 'bar))"
    ]
  , testGroup
    "The First Commandment of cons"
    [ testProperty
        "The cons-expressions are the same (Pair A D) if their cars are the same A and their cdrs are the same D."
      $ property
      $ do
          value <- forAll $ Gen.text (Range.constant 1 10) Gen.alpha
          runInterp ("(= Atom " <> "'" <> value <> " '" <> value <> ")")
            === Right "(the Universe (Atom 'foo 'bar))"
    ]
  , testGroup
    "The Commandment of zero"
    [ testProperty "zero is the same Nat as zero." $ property $ do
        value <- forAll $ Gen.text (Range.constant 1 10) Gen.alpha
        runInterp ("(= Atom " <> "'" <> value <> " '" <> value <> ")")
          === Right "(the Universe (Atom 'foo 'bar))"
    ]
  , testGroup
    "The Commandment of add1"
    [ testProperty
        "If n is the same Nat as k, then (add1 n) is the same Nat as (add1 k)."
      $ property
      $ do
          value <- forAll $ Gen.text (Range.constant 1 10) Gen.alpha
          runInterp ("(= Atom " <> "'" <> value <> " '" <> value <> ")")
            === Right "(the Universe (Atom 'foo 'bar))"
    ]
  , testGroup
    "The Initial First Commandment of lambda"
    [ testProperty
        "Two lambda-expressions that expect the same number of arguments are the same if their bodies are the same after consistently renaming their variables."
      $ property
      $ do
          value <- forAll $ Gen.text (Range.constant 1 10) Gen.alpha
          runInterp ("(= Atom " <> "'" <> value <> " '" <> value <> ")")
            === Right "(the Universe (Atom 'foo 'bar))"
    ]
  , testGroup
    "The Initial Second Commandment of lambda"
    [ testProperty
        "If f is an (-> Y X) then f is the same (-> Y X) as (lambda (y) (f y)) as long as y does not occur in f."
      $ property
      $ do
          value <- forAll $ Gen.text (Range.constant 1 10) Gen.alpha
          runInterp ("(= Atom " <> "'" <> value <> " '" <> value <> ")")
            === Right "(the Universe (Atom 'foo 'bar))"
    ]
  , testGroup
    "The Second Commandment of cons"
    [ testProperty
        "If p is a (Pair A D), then it is the same (Pair A D) as (cons (car p) (cdr p))."
      $ property
      $ do
          value <- forAll $ Gen.text (Range.constant 1 10) Gen.alpha
          runInterp ("(= Atom " <> "'" <> value <> " '" <> value <> ")")
            === Right "(the Universe (Atom 'foo 'bar))"
    ]
  ]
