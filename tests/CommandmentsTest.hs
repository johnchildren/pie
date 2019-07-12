{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module CommandmentsTest
  ( spec_commandments
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

spec_commandments :: Spec
spec_commandments = describe "Commandments related to Pie" $ do
  xdescribe "The Law of Tick Marks"
    $          specify
                 "Two expressions are the same Atom if their values are tick marks followed by identical letters and hyphens"
    $          runInterp "'(= Atom 'foo 'foo)"
    `shouldBe` Right ""

  xdescribe "The First Commandment of cons" $ specify
    "The cons-expressions are the same (Pair A D) if their cars are the same A and their cdrs are the same D."
    pending

  xdescribe "The Commandment of zero"
    $          specify "zero is the same Nat as zero"
    $          runInterp "'(= Nat zero zero)"
    `shouldBe` Right ""

  xdescribe "The Commandment of add1" $ specify
    "If n is the same Nat as k, then (add1 n) is the same Nat as (add1 k)"
    pending

  xdescribe "The Initial First Commandment of lambda" $ specify
    "Two lambda-expressions that expect the same number of arguments are the same if their bodies are the same after consistently renaming their variables"
    pending

  xdescribe "The Initial Second Commandment of lambda" $ specify
    "If f is an (-> Y X) then f is the same (-> Y X) as (lambda (y) (f y)) as long as y does not occur in f"
    pending

  xdescribe "The Second Commandment of cons" $ specify
    "If p is a (Pair A D), then it is the same (Pair A D) as (cons (car p) (cdr p))"
    pending
