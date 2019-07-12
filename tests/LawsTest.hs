{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module LawsTest
  ( spec_laws
  )
where

import           Prelude
import           Control.Effect
import           Control.Effect.Error                     ( runError )
import           Control.Effect.State                     ( evalState )
import           Data.Text                                ( Text )
import           Test.Tasty.Hspec
import           Language.Pie.Interpreter                 ( interp
                                                          , InterpError
                                                          , IState
                                                          )
import qualified Language.Pie.Environment      as Env

runInterp :: Text -> Either InterpError Text
runInterp = run . runError . evalState @IState (Env.empty, Env.empty) . interp

spec_laws :: Spec
spec_laws = describe "Laws related to Pie" $ do
  describe "The Law of Tick Marks"
    $          specify
                 "A tick mark followed by one or more letters and hyphens is an Atom"
    $          runInterp "'letters"
    `shouldBe` Right "(the Atom 'letters)"

  describe "The Law of Atom"
    $          specify "Atom is a type"
    $          runInterp "Atom"
    `shouldBe` Right "(the Universe Atom)"

  xdescribe "The Initial law of Application" $ specify
    "If f is an (-> Y X) and arg is a Y, then (f arg) is an X"
    pending

  xdescribe "The Law of Renaming Variables" $ specify
    "Consistently renaming variables can't change the meaning of anything"
    pending
