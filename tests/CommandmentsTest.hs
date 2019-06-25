module CommandmentsTest
  ( spec_commandments
  )
where

import           Prelude
import           Test.Tasty.Hspec

spec_commandments :: Spec
spec_commandments = describe "Commandments related to Pie" $ do
  describe "The Law of Tick Marks"
    $ specify
        "Two expressions are the same Atom if their values are tick marks followed by identical letters and hyphens"
    $ pending

  describe "The First Commandment of cons"
    $ specify
        "The cons-expressions are the same (Pair A D) if their cars are the same A and their cdrs are the same D."
    $ pending

  describe "The Commandment of zero"
    $ specify "zero is the same Nat as zero"
    $ pending

  describe "The Commandment of add1"
    $ specify
        "If n is the same Nat as k, then (add1 n) is the same Nat as (add1 k)"
    $ pending

  describe "The Initial First Commandment of lambda"
    $ specify
        "Two lambda-expressions that expect the same number of arguments are the same if their bodies are the same after consistently renaming their variables"
    $ pending

  describe "The Initial Second Commandment of lambda"
    $ specify
        "If f is an (-> Y X) then f is the same (-> Y X) as (lambda (y) (f y)) as long as y does not occur in f"
    $ pending

  describe "The Second Commandment of cons"
    $ specify
        "If p is a (Pair A D), then it is the same (Pair A D) as (cons (car p) (cdr p))"
    $ pending
