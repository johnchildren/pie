module Main
  ( main
  )
where

import           Prelude
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.Hedgehog
import           CommandmentsTest
import           EvalTest
import           JudgementsTest
import           LawsTest
import           ParsePrintTest
import           System.IO.Unsafe

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup
  "Tests"
  [ unsafePerformIO (testSpec "commandments" spec_commandments)
  , unsafePerformIO (testSpec "evaluation" spec_eval)
  , unsafePerformIO (testSpec "judgements" spec_judgement)
  , unsafePerformIO (testSpec "laws" spec_laws)
  , testProperty "parse print" hprop_parse_print_trip
  ]
