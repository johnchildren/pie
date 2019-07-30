module JudgementsTest
  ( test_judgements
  )
where

import           Prelude
import           Data.List.NonEmpty                       ( NonEmpty(..) )
import           Data.Text                                ( Text )
import           Test.Tasty
import           Test.Tasty.HUnit
import           Language.Pie.Symbols                     ( Symbol(..)
                                                          , VarName(..)
                                                          )
import qualified Language.Pie.Environment      as Env
import           Language.Pie.Judgement                   ( judgement1
                                                          , judgement2
                                                          , judgement3
                                                          , judgement4
                                                          , Judgement(..)
                                                          )
import           Language.Pie.Expr                        ( Expr(..) )

test_judgements :: TestTree
test_judgements = testGroup
  "Judgement Tests"
  [ testGroup
    "The first form of Judgement"
    [ testCase "checks if an expression if of a type"
    $   judgement1 Env.empty (mkAtom "x") Atom
    @?= Yes
    , testCase
      "checks if a cons of two atoms has the type of a pair of two atom types"
    $   judgement1 Env.empty
                   (Cons (mkAtom "courgette") (mkAtom "baguette"))
                   (Pair Atom Atom)
    @?= Yes
    , testCase "checks if zero is a Nat" $ judgement1 Env.empty Zero Nat @?= Yes
    , testCase "checks if (add1 zero) is a Nat"
    $   judgement1 Env.empty (Add1 Zero) Nat
    @?= Yes
    , testCase "normalises expressions"
    $   judgement1
          Env.empty
          (Car
            (The (Pair Atom Atom) (Cons (mkAtom "courgette") (mkAtom "baguette")))
          )
          Atom
    @?= Yes
    , testCase "applies lambda expressions"
    $   judgement1
          Env.empty
          (App (The (Arrow Atom Atom) (mkLambda "x" (mkVar "x")))
               (mkAtom "foo" :| [])
          )
          Atom
    @?= Yes
    ]
  , testGroup
    "The second form of Judgement"
    [ testCase
      "checks that an atom is the same Atom as an atom that has the same id"
    $   judgement2 Env.empty (mkAtom "courgette") Atom (mkAtom "courgette")
    @?= Yes
    , testCase
      "checks than an atom is a different Atom to an atom wtestCaseh a different id"
    $   judgement2 Env.empty (mkAtom "courgette") Atom (mkAtom "baguette")
    @?= No
    , testCase "checks that zero is the same Nat as zero"
    $   judgement2 Env.empty Zero Nat Zero
    @?= Yes
    , testCase "checks that (add1 zero) is not the same Nat as zero"
    $   judgement2 Env.empty (Add1 Zero) Nat Zero
    @?= No
    , testCase "checks that (add1 zero) is the same Nat as (add1 zero)"
    $   judgement2 Env.empty (Add1 Zero) Nat (Add1 Zero)
    @?= Yes
    , testCase "normalises expressions"
    $ judgement2 Env.empty (Car (Pair (mkAtom "foo") Atom)) Atom (mkAtom "foo")
    @?= Yes
    , testCase "applies lambda expressions"
    $   judgement2 Env.empty
                   (App (mkLambda "x" (mkVar "x")) (mkAtom "foo" :| []))
                   Atom
                   (mkAtom "foo")
    @?= Yes
    ]
  , testGroup
    "The third form of Judgement"
    [ testCase "checks than an atom is not a type"
    $   judgement3 Env.empty (mkAtom "courgette")
    @?= No
    , testCase "checks that Atom is a type" $ judgement3 Env.empty Atom @?= Yes
    , testCase "normalises expressions"
    $   judgement3 Env.empty (Car (Pair Atom Atom))
    @?= Yes
    , testCase "applies lambda expressions"
    $   judgement3 Env.empty (App (mkLambda "x" (mkVar "x")) (Atom :| []))
    @?= Yes
    ]
  , testGroup
    "The fourth form of Judgement"
    [ testCase
      "checks that two atoms wtestCaseh the same id are not the same type"
    $   judgement4 Env.empty (mkAtom "courgette") (mkAtom "courgette")
    @?= No
    , testCase "checks that two Atom types are the same type"
    $   judgement4 Env.empty Atom Atom
    @?= Yes
    , testCase "normalises expressions"
    $   judgement4 Env.empty (Car (Pair Atom Atom)) Atom
    @?= Yes
    , testCase "applies lambda expressions"
    $   judgement4 Env.empty (App (mkLambda "x" (mkVar "x")) (Atom :| [])) Atom
    @?= Yes
    ]
  ]

mkAtom :: Text -> Expr
mkAtom = Quote . Symbol

mkVar :: Text -> Expr
mkVar = Var . flip VarName 0

mkLambda :: Text -> Expr -> Expr
mkLambda x = Lambda (VarName x 0 :| [])
