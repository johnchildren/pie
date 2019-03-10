{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           Data.Text                                ( Text )
import           Hedgehog                          hiding ( Var )
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.Hedgehog                      ( testProperty )
import           Language.Pie.Symbols                     ( Symbol(..)
                                                          , VarName(..)
                                                          )
import           Language.Pie.Parse                       ( parsePie )
import           Language.Pie.Print                       ( printPie )
import qualified Language.Pie.Environment      as Env
import           Language.Pie.Eval                        ( evalPie )
import           Language.Pie.Judgement                   ( judgement1
                                                          , judgement2
                                                          , judgement3
                                                          , judgement4
                                                          , Judgement(..)
                                                          )
import           Language.Pie.Expr                        ( Expr(..) )


genVarName :: Gen VarName
genVarName = VarName <$> Gen.text (Range.constant 1 10) Gen.alpha

genSymbol :: Gen Symbol
genSymbol = Symbol <$> Gen.text (Range.constant 1 10) Gen.alpha

genExprs :: Gen Expr
genExprs = Gen.recursive
  Gen.choice
  [ Var <$> genVarName
  , Gen.constant Atom
  , Quote <$> genSymbol
  , Gen.constant Nat
  , Gen.constant Zero
  , Gen.constant Universe
  ]
  [ Gen.subterm2 genExprs genExprs The
  , Gen.subterm2 genExprs genExprs Cons
  , Gen.subterm2 genExprs genExprs Pair
  , Gen.subterm genExprs Car
  , Gen.subterm genExprs Cdr
--  , Gen.subtermM2 genExprs
--                  genExprs
--                  (\x y -> Pie <$> genVarName <*> pure x <*> pure y)
  , Gen.subterm2 genExprs genExprs Arrow
  , Gen.subtermM genExprs (\x -> Lambda <$> genVarName <*> pure x)
  , Gen.subterm2 genExprs genExprs App
  , Gen.subterm genExprs Add1
  , Gen.subterm3 genExprs genExprs genExprs WhichNat
  , Gen.subterm3 genExprs genExprs genExprs IterNat
  , Gen.subterm3 genExprs genExprs genExprs RecNat
  ]


prop_parse_print_trip :: Property
prop_parse_print_trip = withTests 1000 . property $ do
  exprs <- forAll genExprs
  tripping exprs printPie parsePie

main :: IO ()
main = do
  mySpec <- testSpec "specs" spec
  defaultMain
    (testGroup "tests"
               [mySpec, testProperty "parse print" prop_parse_print_trip]
    )

spec :: Spec
spec = do
  describe "Evaluating pie expression" $ do
    describe "normalisation" $ do
      it "normalises expressions"
        $          evalPie
                     Env.empty
                     (Car
                       (Cons (Cons (mkAtom "aubergine") (mkAtom "courgette"))
                             (mkAtom "tomato")
                       )
                     )
        `shouldBe` Right (Cons (mkAtom "aubergine") (mkAtom "courgette"))

      it "normalises expression of types and values"
        $          evalPie
                     Env.empty
                     (Pair (Car (Cons Atom (mkAtom "olive")))
                           (Cdr (Cons (mkAtom "oil") Atom))
                     )
        `shouldBe` Right (Pair Atom Atom)

    describe "lambda expressions" $ do
      it "can apply lambda expressions"
        $ evalPie Env.empty (App (Lambda (VarName "x") (mkVar "x")) Atom)
        `shouldBe` Right Atom

      it "will normalise while applying a lambda expression"
        $          evalPie
                     Env.empty
                     (App
                       (Lambda (VarName "x") (Car (Cons (mkVar "x") (mkAtom "foo"))))
                       Atom
                     )
        `shouldBe` Right Atom

      it "will ignore unused variables"
        $ evalPie Env.empty (App (Lambda (VarName "x") (mkAtom "foo")) Atom)
        `shouldBe` Right (mkAtom "foo")

      it "works with nested lambda applications"
        $          evalPie
                     Env.empty
                     (App
                       (App (Lambda (VarName "x") (Lambda (VarName "y") (mkVar "x")))
                            Atom
                       )
                       (mkAtom "foo")
                     )
        `shouldBe` Right Atom

    describe "which-Nat" $ do
      it "evaluates to base when target is zero"
        $          evalPie
                     Env.empty
                     (WhichNat Zero
                               (mkAtom "naught")
                               (Lambda (VarName "n") (mkAtom "more"))
                     )
        `shouldBe` Right (mkAtom "naught")

      it "evaluates to step n when target is (add1 n)"
        $          evalPie
                     Env.empty
                     (WhichNat (Add1 (Add1 (Add1 (Add1 Zero))))
                               (mkAtom "naught")
                               (Lambda (VarName "n") (mkAtom "more"))
                     )
        `shouldBe` Right (mkAtom "more")

    describe "iter-Nat" $ do
      it "evaluates to base when target is zero"
        $          evalPie
                     Env.empty
                     (IterNat Zero
                              (mkAtom "naught")
                              (Lambda (VarName "n") (mkAtom "more"))
                     )
        `shouldBe` Right (mkAtom "naught")

      it "evaluates to step n when target is (add1 n)"
        $          evalPie
                     Env.empty
                     (IterNat (Add1 (Add1 (Add1 (Add1 Zero))))
                              (mkAtom "naught")
                              (Lambda (VarName "n") (mkAtom "more"))
                     )
        `shouldBe` Right (mkAtom "more")

      it "each add1 in the value of target is replaced by a step"
        $          evalPie
                     Env.empty
                     (IterNat (Add1 (Add1 (Add1 (Add1 (Add1 Zero)))))
                              (Add1 (Add1 (Add1 Zero)))
                              (Lambda (VarName "smaller") (Add1 (mkVar "smaller")))
                     )
        `shouldBe` Right
                     (Add1 (Add1 (Add1 (Add1 (Add1 (Add1 (Add1 (Add1 Zero)))))))
                     )

    describe "rec-Nat" $ do
      it "evaluates to base when target is zero"
        $          evalPie
                     Env.empty
                     (RecNat
                       Zero
                       (mkAtom "naught")
                       (Lambda (VarName "n") (Lambda (VarName "i") (mkAtom "more")))
                     )
        `shouldBe` Right (mkAtom "naught")

      it "evaluates to (step n (iter-Nat n base step)) when target is (add1 n)"
        $          evalPie
                     Env.empty
                     (RecNat
                       (Add1 (Add1 (Add1 (Add1 Zero))))
                       (mkAtom "naught")
                       (Lambda (VarName "n") (Lambda (VarName "i") (mkAtom "more")))
                     )
        `shouldBe` Right (mkAtom "more")

  describe "The first form of Judgement" $ do
    it "checks if an expression if of a type"
      $          judgement1 Env.empty (mkAtom "x") Atom
      `shouldBe` Yes

    it "checks if a cons of two atoms has the type of a pair of two atom types"
      $          judgement1 Env.empty
                            (Cons (mkAtom "courgette") (mkAtom "baguette"))
                            (Pair Atom Atom)
      `shouldBe` Yes

    it "checks if zero is a Nat" $ judgement1 Env.empty Zero Nat `shouldBe` Yes

    it "checks if (add1 zero) is a Nat"
      $          judgement1 Env.empty (Add1 Zero) Nat
      `shouldBe` Yes

    it "normalises expressions"
      $ judgement1 Env.empty
                   (Car (Cons (mkAtom "courgette") (mkAtom "baguette")))
                   Atom
      `shouldBe` Yes

    it "applies lambda expressions"
      $ judgement1 Env.empty
                   (App (Lambda (VarName "x") (mkVar "x")) (mkAtom "foo"))
                   Atom
      `shouldBe` Yes

  describe "The second form of Judgement" $ do
    it "checks that an atom is the same Atom as an atom that has the same id"
      $ judgement2 Env.empty (mkAtom "courgette") Atom (mkAtom "courgette")
      `shouldBe` Yes

    it "checks than an atom is a different Atom to an atom with a different id"
      $ judgement2 Env.empty (mkAtom "courgette") Atom (mkAtom "baguette")
      `shouldBe` No

    it "checks that zero is the same Nat as zero"
      $          judgement2 Env.empty Zero Nat Zero
      `shouldBe` Yes

    it "checks that (add1 zero) is not the same Nat as zero"
      $          judgement2 Env.empty (Add1 Zero) Nat Zero
      `shouldBe` No

    it "checks that (add1 zero) is the same Nat as (add1 zero)"
      $          judgement2 Env.empty (Add1 Zero) Nat (Add1 Zero)
      `shouldBe` Yes

    it "normalises expressions"
      $          judgement2 Env.empty
                            (Car (Pair (mkAtom "foo") Atom))
                            Atom
                            (mkAtom "foo")
      `shouldBe` Yes

    it "applies lambda expressions"
      $ judgement2 Env.empty
                   (App (Lambda (VarName "x") (mkVar "x")) (mkAtom "foo"))
                   Atom
                   (mkAtom "foo")
      `shouldBe` Yes

  describe "The third form of Judgement" $ do
    it "checks than an atom is not a type"
      $          judgement3 Env.empty (mkAtom "courgette")
      `shouldBe` No

    it "checks that Atom is a type" $ judgement3 Env.empty Atom `shouldBe` Yes

    it "normalises expressions"
      $          judgement3 Env.empty (Car (Pair Atom Atom))
      `shouldBe` Yes

    it "applies lambda expressions"
      $ judgement3 Env.empty (App (Lambda (VarName "x") (mkVar "x")) Atom)
      `shouldBe` Yes

  describe "The fourth form of Judgement" $ do
    it "checks that two atoms with the same id are not the same type"
      $          judgement4 Env.empty (mkAtom "courgette") (mkAtom "courgette")
      `shouldBe` No

    it "checks that two Atom types are the same type"
      $          judgement4 Env.empty Atom Atom
      `shouldBe` Yes

    it "normalises expressions"
      $          judgement4 Env.empty (Car (Pair Atom Atom)) Atom
      `shouldBe` Yes

    it "applies lambda expressions"
      $ judgement4 Env.empty (App (Lambda (VarName "x") (mkVar "x")) Atom) Atom
      `shouldBe` Yes

mkAtom :: Text -> Expr
mkAtom = Quote . Symbol

mkVar :: Text -> Expr
mkVar = Var . VarName
