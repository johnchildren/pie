module Main
  ( main
  )
where

import           Test.Hspec
import           Language.Pie.Parse                       ( parsePie )
import           Language.Pie.Print                       ( printPie )
import           Language.Pie.Eval                        ( evalPie
                                                          , emptyEnv
                                                          )
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

mkAtom :: String -> Expr
mkAtom = AtomData . AtomID

mkVar :: String -> Expr
mkVar = Var . VarName

main :: IO ()
main = hspec $ do
  describe "Parsing pie expressions" $ do
    it "can parse the type Atom" $ parsePie "Atom" `shouldBe` Right AtomType

    it "can parse an Atom" $ parsePie "'courgette" `shouldBe` Right
      (mkAtom "courgette")

    it "can parse a Pair of two Atoms"
      $          parsePie "(Pair Atom Atom)"
      `shouldBe` Right (Pair AtomType AtomType)

    it "can parse a Lambda Expression"
      $          parsePie "(lambda (x) (cons x 'courgette))"
      `shouldBe` Right
                   (Lambda (VarName "x") (Cons (mkVar "x") (mkAtom "courgette"))
                   )

  describe "Printing pie expressions" $ do
    it "can print a Cons"
      $          printPie (Cons (mkAtom "courgette") (mkAtom "baguette"))
      `shouldBe` "(cons 'courgette 'baguette)"

    it "can print a Pair"
      $          printPie (Pair AtomType AtomType)
      `shouldBe` "(Pair Atom Atom)"

    it "can print four"
      $          printPie (Add1 (Add1 (Add1 (Add1 Zero))))
      `shouldBe` "(add1 (add1 (add1 (add1 zero))))"

  describe "Evaluating pie expression" $ do
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

    it "can apply lambda expressions"
      $ evalPie emptyEnv (App (Lambda (VarName "x") (mkVar "x")) AtomType)
      `shouldBe` Right AtomType

    it "will normalise while applying a lambda expression"
      $          evalPie
                   emptyEnv
                   (App (Lambda (VarName "x") (Car (Cons (mkVar "x") (mkAtom "foo"))))
                        AtomType
                   )
      `shouldBe` Right AtomType

  describe "The first form of Judgement" $ do
    it "checks if an expression if of a type"
      $          judgement1 emptyEnv (mkAtom "x") AtomType
      `shouldBe` Yes

    it "checks if a cons of two atoms has the type of a pair of two atom types"
      $          judgement1 emptyEnv
                            (Cons (mkAtom "courgette") (mkAtom "baguette"))
                            (Pair AtomType AtomType)
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