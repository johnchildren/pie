module Main
  ( main
  )
where

import           Prelude
import           Test.Tasty
import           Test.Tasty.Hedgehog
--import           CommandmentsTest
import           EvalTest
import           JudgementsTest
import           LawsTest
import           ParsePrintTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup
  "Tests"
  [ --test_commandments -- disabled until `=` is implemented
  test_eval
  , test_judgements
  , test_laws
  , testProperty "parse print" hprop_parse_print_trip
  , testProperty "to from core" hprop_to_from_core_trip
  ]
