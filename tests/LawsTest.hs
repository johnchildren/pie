{-# LANGUAGE LambdaCase #-}

module LawsTest
  ( spec_laws
  )
where

import           Prelude
import           Control.Monad.Trans.State.Strict         ( evalStateT )
import           Control.Monad.Trans.Except               ( runExceptT )
import           Data.Text                                ( Text )
import           Test.Tasty.Hspec
import qualified Language.Pie.Interpreter      as Interp
import qualified Language.Pie.Environment      as Env

spec_laws :: Spec
spec_laws = describe "Laws related to Pie" $ do
  describe "The Law of Tick Marks"
    $ specify
        "A tick mark followed by one or more letters and hyphens is an Atom"
    $ pending

  describe "The Law of Atom" $ specify "Atom is a type" $ pending

  describe "The Initial law of Application"
    $ specify "If f is an (-> Y X) and arg is a Y, then (f arg) is an X"
    $ pending

  describe "The Law of Renaming Variables"
    $ specify
        "Consistently renaming variables can't change the meaning of anything"
    $ pending
