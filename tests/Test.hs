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
import           Language.Pie.Eval                        ( val )
import           Language.Pie.Judgement                   ( judgement1
                                                          , judgement2
                                                          , judgement3
                                                          , judgement4
                                                          , Judgement(..)
                                                          )
import           Language.Pie.Values                      ( Value(..) )
import           Language.Pie.Expr                        ( Expr(..)
                                                          , Clos(..)
                                                          )


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
  , Gen.subtermM genExprs (\x -> Lambda <$> genVarName <*> pure (Clos x))
  , Gen.subterm2 genExprs genExprs App
  , Gen.subterm genExprs Add1
  , Gen.subterm3 genExprs genExprs genExprs (\x y z -> WhichNat x y (Clos z))
  , Gen.subterm3 genExprs genExprs genExprs (\x y z -> IterNat x y (Clos z))
  , Gen.subterm3 genExprs genExprs genExprs (\x y z -> RecNat x y (Clos z))
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
        $          val
                     Env.empty
                     (Car
                       (Cons (Cons (mkAtom "aubergine") (mkAtom "courgette"))
                             (mkAtom "tomato")
                       )
                     )
        `shouldBe` Right (CONS (mkATOM "aubergine") (mkATOM "courgette"))

      it "normalises expression of types and values"
        $          val
                     Env.empty
                     (Pair (Car (Cons Atom (mkAtom "olive")))
                           (Cdr (Cons (mkAtom "oil") Atom))
                     )
        `shouldBe` Right (PAIR ATOM ATOM)

    describe "lambda expressions" $ do
      it "can apply lambda expressions"
        $          val Env.empty (App (mkLambda "x" (mkVar "x")) Atom)
        `shouldBe` Right ATOM

      it "will normalise while applying a lambda expression"
        $          val
                     Env.empty
                     (App (mkLambda "x" (Car (Cons (mkVar "x") (mkAtom "foo")))) Atom)
        `shouldBe` Right ATOM

      it "will ignore unused variables"
        $          val Env.empty (App (mkLambda "x" (mkAtom "foo")) Atom)
        `shouldBe` Right (mkATOM "foo")

      it "works with nested lambda applications"
        $          val
                     Env.empty
                     (App (App (mkLambda "x" (mkLambda "y" (mkVar "x"))) Atom)
                          (mkAtom "foo")
                     )
        `shouldBe` Right ATOM

    describe "which-Nat" $ do
      it "evaluates to base when target is zero"
        $          val
                     Env.empty
                     (WhichNat Zero
                               (mkAtom "naught")
                               (Clos (mkLambda "n" (mkAtom "more")))
                     )
        `shouldBe` Right (mkATOM "naught")

      it "evaluates to step n when target is (add1 n)"
        $          val
                     Env.empty
                     (WhichNat (Add1 (Add1 (Add1 (Add1 Zero))))
                               (mkAtom "naught")
                               (Clos (mkLambda "n" (mkAtom "more")))
                     )
        `shouldBe` Right (mkATOM "more")

    describe "iter-Nat" $ do
      it "evaluates to base when target is zero"
        $          val
                     Env.empty
                     (IterNat Zero
                              (mkAtom "naught")
                              (Clos (mkLambda "n" (mkAtom "more")))
                     )
        `shouldBe` Right (mkATOM "naught")

      it "evaluates to step n when target is (add1 n)"
        $          val
                     Env.empty
                     (IterNat (Add1 (Add1 (Add1 (Add1 Zero))))
                              (mkAtom "naught")
                              (Clos (mkLambda "n" (mkAtom "more")))
                     )
        `shouldBe` Right (mkATOM "more")

      it "each add1 in the value of target is replaced by a step"
        $          val
                     Env.empty
                     (IterNat (Add1 (Add1 (Add1 (Add1 (Add1 Zero)))))
                              (Add1 (Add1 (Add1 Zero)))
                              (Clos (mkLambda "smaller" (Add1 (mkVar "smaller"))))
                     )
        `shouldBe` Right
                     (ADD1 (ADD1 (ADD1 (ADD1 (ADD1 (ADD1 (ADD1 (ADD1 ZERO)))))))
                     )

    describe "rec-Nat" $ do
      it "evaluates to base when target is zero"
        $          val
                     Env.empty
                     (RecNat Zero
                             (mkAtom "naught")
                             (Clos (mkLambda "n" (mkLambda "i" (mkAtom "more"))))
                     )
        `shouldBe` Right (mkATOM "naught")

      it "evaluates to (step n (iter-Nat n base step)) when target is (add1 n)"
        $          val
                     Env.empty
                     (RecNat (Add1 (Add1 (Add1 (Add1 Zero))))
                             (mkAtom "naught")
                             (Clos (mkLambda "n" (mkLambda "i" (mkAtom "more"))))
                     )
        `shouldBe` Right (mkATOM "more")

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
      $          judgement1 Env.empty
                            (App (mkLambda "x" (mkVar "x")) (mkAtom "foo"))
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
      $          judgement2 Env.empty
                            (App (mkLambda "x" (mkVar "x")) (mkAtom "foo"))
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
      $          judgement3 Env.empty (App (mkLambda "x" (mkVar "x")) Atom)
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
      $          judgement4 Env.empty (App (mkLambda "x" (mkVar "x")) Atom) Atom
      `shouldBe` Yes

mkAtom :: Text -> Expr
mkAtom = Quote . Symbol

mkVar :: Text -> Expr
mkVar = Var . VarName

mkLambda :: Text -> Expr -> Expr
mkLambda x b = Lambda (VarName x) (Clos b)

mkATOM :: Text -> Value
mkATOM = QUOTE . Symbol
