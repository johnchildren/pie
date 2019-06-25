module Test
  ( spec_eval
  , spec_judgement
  )
where

import           Prelude
import           Data.Text                                ( Text )
import           Test.Tasty.Hspec
import           Language.Pie.Symbols                     ( Symbol(..)
                                                          , VarName(..)
                                                          )
import qualified Language.Pie.Environment      as Env
import           Language.Pie.Eval                        ( val
                                                          , EvalError
                                                          )
import           Language.Pie.Judgement                   ( judgement1
                                                          , judgement2
                                                          , judgement3
                                                          , judgement4
                                                          , Judgement(..)
                                                          )
import           Language.Pie.Values                      ( Value(..)
                                                          , Closure(..)
                                                          )
import           Language.Pie.Expr                        ( Expr(..)
                                                          , CoreExpr(..)
                                                          , toCore
                                                          )

evalExpr :: Expr -> Either EvalError Value
evalExpr = val Env.empty . toCore

spec_eval :: Spec
spec_eval = describe "Evaluating pie expression" $ do
  describe "normalisation" $ do
    it "normalises expressions"
      $          evalExpr
                   (Car
                     (Cons (Cons (mkAtom "aubergine") (mkAtom "courgette"))
                           (mkAtom "tomato")
                     )
                   )
      `shouldBe` Right (VCons (mkAtomVal "aubergine") (mkAtomVal "courgette"))

    it "normalises expression of types and values"
      $          evalExpr
                   (Pair (Car (Cons Atom (mkAtom "olive")))
                         (Cdr (Cons (mkAtom "oil") Atom))
                   )
      `shouldBe` Right
                   (VSigma
                     VAtom
                     (CLOS Env.empty
                           (Dimmed "x")
                           (CCdr (CCons (mkCoreAtom "oil") CAtom))
                     )
                   )

  describe "lambda expressions" $ do
    it "can apply lambda expressions"
      $          evalExpr (App (mkLambda "x" (mkVar "x")) Atom)
      `shouldBe` Right VAtom

    it "will normalise while applying a lambda expression"
      $          evalExpr
                   (App (mkLambda "x" (Car (Cons (mkVar "x") (mkAtom "foo")))) Atom)
      `shouldBe` Right VAtom

    it "will ignore unused variables"
      $          evalExpr (App (mkLambda "x" (mkAtom "foo")) Atom)
      `shouldBe` Right (mkAtomVal "foo")

    it "works with nested lambda applications"
      $          evalExpr
                   (App (App (mkLambda "x" (mkLambda "y" (mkVar "x"))) Atom)
                        (mkAtom "foo")
                   )
      `shouldBe` Right VAtom

  describe "which-Nat" $ do
    it "evaluates to base when target is zero"
      $          evalExpr
                   (WhichNat Zero (mkAtom "naught") (mkLambda "n" (mkAtom "more")))
      `shouldBe` Right (mkAtomVal "naught")

    it "evaluates to step n when target is (add1 n)"
      $          evalExpr
                   (WhichNat (Add1 (Add1 (Add1 (Add1 Zero))))
                             (mkAtom "naught")
                             (mkLambda "n" (mkAtom "more"))
                   )
      `shouldBe` Right (mkAtomVal "more")

  describe "iter-Nat" $ do
    it "evaluates to base when target is zero"
      $ evalExpr (IterNat Zero (mkAtom "naught") (mkLambda "n" (mkAtom "more")))
      `shouldBe` Right (mkAtomVal "naught")

    it "evaluates to step n when target is (add1 n)"
      $          evalExpr
                   (IterNat (Add1 (Add1 (Add1 (Add1 Zero))))
                            (mkAtom "naught")
                            (mkLambda "n" (mkAtom "more"))
                   )
      `shouldBe` Right (mkAtomVal "more")

    it "each add1 in the value of target is replaced by a step"
      $          evalExpr
                   (IterNat (Add1 (Add1 (Add1 (Add1 (Add1 Zero)))))
                            (Add1 (Add1 (Add1 Zero)))
                            (mkLambda "smaller" (Add1 (mkVar "smaller")))
                   )
      `shouldBe` Right
                   (VAdd1
                     (VAdd1
                       (VAdd1 (VAdd1 (VAdd1 (VAdd1 (VAdd1 (VAdd1 VZero))))))
                     )
                   )

  describe "rec-Nat" $ do
    it "evaluates to base when target is zero"
      $          evalExpr
                   (RecNat Zero
                           (mkAtom "naught")
                           (mkLambda "n" (mkLambda "i" (mkAtom "more")))
                   )
      `shouldBe` Right (mkAtomVal "naught")

    it "evaluates to (step n (iter-Nat n base step)) when target is (add1 n)"
      $          evalExpr
                   (RecNat (Add1 (Add1 (Add1 (Add1 Zero))))
                           (mkAtom "naught")
                           (mkLambda "n" (mkLambda "i" (mkAtom "more")))
                   )
      `shouldBe` Right (mkAtomVal "more")

spec_judgement :: Spec
spec_judgement = do
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
      $          judgement1
                   Env.empty
                   (Car
                     (The (Pair Atom Atom)
                          (Cons (mkAtom "courgette") (mkAtom "baguette"))
                     )
                   )
                   Atom
      `shouldBe` Yes

    it "applies lambda expressions"
      $          judgement1
                   Env.empty
                   (App (The (Arrow Atom Atom) (mkLambda "x" (mkVar "x"))) (mkAtom "foo")
                   )
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
mkLambda x = Lambda (VarName x)

mkAtomVal :: Text -> Value
mkAtomVal = VQuote . Symbol

mkCoreAtom :: Text -> CoreExpr
mkCoreAtom = CQuote . Symbol
