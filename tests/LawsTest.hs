{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module LawsTest
  ( test_laws
  )
where

import           Prelude
import           Control.Effect
import           Control.Effect.Error                     ( runError )
import           Control.Effect.State                     ( evalState )
import           Data.Text                                ( Text )
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit
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

test_laws :: TestTree
test_laws = testGroup
  "Commandments related to Pie"
  [ testGroup
    "The Law of Tick Marks"
    [ testProperty
        "A tick mark followed by one or more letters and hyphens is an Atom."
      $ property
      $ do
          value <- forAll $ Gen.text (Range.constant 1 10) Gen.alpha
          runInterp ("'" <> value) === (Right $ "(the Atom '" <> value <> ")")
    ]
  , testGroup
    "The Law of Atom"
    [ testCase "Atom is a type." $ runInterp "Atom" @?= Right
        "(the Universe Atom)"
    ]
  , testGroup
    "The Initial law of Application"
    [ testProperty "If f is an (-> Y X) and arg is a Y, then (f arg) is an X."
      $ property
      $ do
          value <- forAll $ Gen.text (Range.constant 1 10) Gen.alpha
          runInterp ("'" <> value) === (Right $ "(the Atom '" <> value <> ")")
    ]
  , testGroup
    "The Law of Renaming Variables"
    [ testProperty
        "Consistently renaming variables can't change the meaning of anything."
      $ property
      $ do
          value <- forAll $ Gen.text (Range.constant 1 10) Gen.alpha
          runInterp ("'" <> value) === (Right $ "(the Atom '" <> value <> ")")
    ]
  ]
