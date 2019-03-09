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
import           Language.Pie.Parse                       ( parsePie )
import           Language.Pie.Print                       ( printPie )
import           Language.Pie.Environment                 ( emptyEnv )
import           Language.Pie.Eval                        ( evalPie )
import           Language.Pie.Judgement                   ( judgement1
                                                          , judgement2
                                                          , judgement3
                                                          , judgement4
                                                          , Judgement(..)
                                                          )
import           Language.Pie.Expr                        ( AtomID(..)
                                                          , VarName(..)
                                                          , Expr(..)
                                                          )


genVarName :: Gen VarName
genVarName = VarName <$> Gen.text (Range.constant 1 10) Gen.alpha

genAtomID :: Gen AtomID
genAtomID = AtomID <$> Gen.text (Range.constant 1 10) Gen.alpha

genExprs :: Gen Expr
genExprs = Gen.recursive
  Gen.choice
  [ Var <$> genVarName
  , Gen.constant AtomType
  , AtomData <$> genAtomID
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
                     emptyEnv
                     (Car
                       (Cons (Cons (mkAtom "aubergine") (mkAtom "courgette"))
                             (mkAtom "tomato")
                       )
                     )
        `shouldBe` Right (Cons (mkAtom "aubergine") (mkAtom "courgette"))

      it "normalises expression of types and values"
        $          evalPie
                     emptyEnv
                     (Pair (Car (Cons AtomType (mkAtom "olive")))
                           (Cdr (Cons (mkAtom "oil") AtomType))
                     )
        `shouldBe` Right (Pair AtomType AtomType)

    describe "lambda expressions" $ do
      it "can apply lambda expressions"
        $ evalPie emptyEnv (App (Lambda (VarName "x") (mkVar "x")) AtomType)
        `shouldBe` Right AtomType

      it "will normalise while applying a lambda expression"
        $          evalPie
                     emptyEnv
                     (App
                       (Lambda (VarName "x") (Car (Cons (mkVar "x") (mkAtom "foo"))))
                       AtomType
                     )
        `shouldBe` Right AtomType

      it "will ignore unused variables"
        $ evalPie emptyEnv (App (Lambda (VarName "x") (mkAtom "foo")) AtomType)
        `shouldBe` Right (mkAtom "foo")

      it "works with nested lambda applications"
        $          evalPie
                     emptyEnv
                     (App
                       (App (Lambda (VarName "x") (Lambda (VarName "y") (mkVar "x")))
                            AtomType
                       )
                       (mkAtom "foo")
                     )
        `shouldBe` Right AtomType

    describe "which-Nat" $ do
      it "evaluates to base when target is zero"
        $          evalPie
                     emptyEnv
                     (WhichNat Zero
                               (mkAtom "naught")
                               (Lambda (VarName "n") (mkAtom "more"))
                     )
        `shouldBe` Right (mkAtom "naught")

      it "evaluates to step n when target is (add1 n)"
        $          evalPie
                     emptyEnv
                     (WhichNat (Add1 (Add1 (Add1 (Add1 Zero))))
                               (mkAtom "naught")
                               (Lambda (VarName "n") (mkAtom "more"))
                     )
        `shouldBe` Right (mkAtom "more")

    describe "iter-Nat" $ do
      it "evaluates to base when target is zero"
        $          evalPie
                     emptyEnv
                     (IterNat Zero
                              (mkAtom "naught")
                              (Lambda (VarName "n") (mkAtom "more"))
                     )
        `shouldBe` Right (mkAtom "naught")

      it "evaluates to step n when target is (add1 n)"
        $          evalPie
                     emptyEnv
                     (IterNat (Add1 (Add1 (Add1 (Add1 Zero))))
                              (mkAtom "naught")
                              (Lambda (VarName "n") (mkAtom "more"))
                     )
        `shouldBe` Right (mkAtom "more")

      it "each add1 in the value of target is replaced by a step"
        $          evalPie
                     emptyEnv
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
                     emptyEnv
                     (RecNat
                       Zero
                       (mkAtom "naught")
                       (Lambda (VarName "n") (Lambda (VarName "i") (mkAtom "more")))
                     )
        `shouldBe` Right (mkAtom "naught")

      it "evaluates to (step n (iter-Nat n base step)) when target is (add1 n)"
        $          evalPie
                     emptyEnv
                     (RecNat
                       (Add1 (Add1 (Add1 (Add1 Zero))))
                       (mkAtom "naught")
                       (Lambda (VarName "n") (Lambda (VarName "i") (mkAtom "more")))
                     )
        `shouldBe` Right (mkAtom "more")

  describe "The first form of Judgement" $ do
    it "checks if an expression if of a type"
      $          judgement1 emptyEnv (mkAtom "x") AtomType
      `shouldBe` Yes

    it "checks if a cons of two atoms has the type of a pair of two atom types"
      $          judgement1 emptyEnv
                            (Cons (mkAtom "courgette") (mkAtom "baguette"))
                            (Pair AtomType AtomType)
      `shouldBe` Yes

    it "checks if zero is a Nat" $ judgement1 emptyEnv Zero Nat `shouldBe` Yes

    it "checks if (add1 zero) is a Nat"
      $          judgement1 emptyEnv (Add1 Zero) Nat
      `shouldBe` Yes

    it "normalises expressions"
      $ judgement1 emptyEnv
                   (Car (Cons (mkAtom "courgette") (mkAtom "baguette")))
                   AtomType
      `shouldBe` Yes

    it "applies lambda expressions"
      $ judgement1 emptyEnv
                   (App (Lambda (VarName "x") (mkVar "x")) (mkAtom "foo"))
                   AtomType
      `shouldBe` Yes

  describe "The second form of Judgement" $ do
    it "checks that an atom is the same Atom as an atom that has the same id"
      $ judgement2 emptyEnv (mkAtom "courgette") AtomType (mkAtom "courgette")
      `shouldBe` Yes

    it "checks than an atom is a different Atom to an atom with a different id"
      $ judgement2 emptyEnv (mkAtom "courgette") AtomType (mkAtom "baguette")
      `shouldBe` No

    it "checks that zero is the same Nat as zero"
      $          judgement2 emptyEnv Zero Nat Zero
      `shouldBe` Yes

    it "checks that (add1 zero) is not the same Nat as zero"
      $          judgement2 emptyEnv (Add1 Zero) Nat Zero
      `shouldBe` No

    it "checks that (add1 zero) is the same Nat as (add1 zero)"
      $          judgement2 emptyEnv (Add1 Zero) Nat (Add1 Zero)
      `shouldBe` Yes

    it "normalises expressions"
      $          judgement2 emptyEnv
                            (Car (Pair (mkAtom "foo") AtomType))
                            AtomType
                            (mkAtom "foo")
      `shouldBe` Yes

    it "applies lambda expressions"
      $ judgement2 emptyEnv
                   (App (Lambda (VarName "x") (mkVar "x")) (mkAtom "foo"))
                   AtomType
                   (mkAtom "foo")
      `shouldBe` Yes

  describe "The third form of Judgement" $ do
    it "checks than an atom is not a type"
      $          judgement3 emptyEnv (mkAtom "courgette")
      `shouldBe` No

    it "checks that Atom is a type"
      $          judgement3 emptyEnv AtomType
      `shouldBe` Yes

    it "normalises expressions"
      $          judgement3 emptyEnv (Car (Pair AtomType AtomType))
      `shouldBe` Yes

    it "applies lambda expressions"
      $ judgement3 emptyEnv (App (Lambda (VarName "x") (mkVar "x")) AtomType)
      `shouldBe` Yes

  describe "The fourth form of Judgement" $ do
    it "checks that two atoms with the same id are not the same type"
      $          judgement4 emptyEnv (mkAtom "courgette") (mkAtom "courgette")
      `shouldBe` No

    it "checks that two Atom types are the same type"
      $          judgement4 emptyEnv AtomType AtomType
      `shouldBe` Yes

    it "normalises expressions"
      $          judgement4 emptyEnv (Car (Pair AtomType AtomType)) AtomType
      `shouldBe` Yes

    it "applies lambda expressions"
      $          judgement4 emptyEnv
                            (App (Lambda (VarName "x") (mkVar "x")) AtomType)
                            AtomType
      `shouldBe` Yes

mkAtom :: Text -> Expr
mkAtom = AtomData . AtomID

mkVar :: Text -> Expr
mkVar = Var . VarName
