{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Data.Functor.Foldable                    ( Fix(..) )
import           Language.Pie.Parse                       ( parsePie )
import           Language.Pie.Print                       ( printPie )
import           Language.Pie.Eval                        ( evalPie )
import           Language.Pie.Expr                        ( AtomID(..)
                                                          , VarName(..)
                                                          , Expr(..)
                                                          )

main :: IO ()
main = hspec $ do
  describe "Parse" $ do
    it "can parse the type Atom" $
      parsePie "Atom" `shouldBe` Right AtomType

    it "can parse an Atom" $
      parsePie "'courgette" `shouldBe` Right (AtomData (AtomID "courgette"))

    it "can parse a Pair of two Atoms" $
      parsePie "(Pair Atom Atom)" `shouldBe` Right (Pair AtomType AtomType)

    it "can parse a Lambda Expression" $
      parsePie "(lambda (x) (cons x 'courgette))" `shouldBe` Right
        (Lambda (VarName "x")
                (Cons (Var (VarName "x")) (AtomData (AtomID "courgette")))
        )

  describe "Print" $ do
    it "can print a Cons" $
      printPie
          (Cons (AtomData (AtomID "courgette")) (AtomData (AtomID "baguette")))
        `shouldBe` "(cons 'courgette 'baguette)"

    it "can print a Pair" $
      printPie (Pair AtomType AtomType) `shouldBe` "(Pair Atom Atom)"

    it "can print four" $
      printPie (Add1 (Add1 (Add1 (Add1 Zero))))
        `shouldBe` "(add1 (add1 (add1 (add1 zero))))"

  describe "Eval" $ do
    it "normalises expressions" $
      evalPie
          (Car
            (Cons
              (Cons (AtomData (AtomID "aubergine"))
                    (AtomData (AtomID "courgette"))
              )
              (AtomData (AtomID "tomato"))
            )
          )
        `shouldBe` Right
                     (Cons (AtomData (AtomID "aubergine"))
                           (AtomData (AtomID "courgette"))
                     )

    it "normalises expression of types and values" $
      evalPie
          (Pair (Car (Cons AtomType (AtomData (AtomID "olive"))))
                (Cdr (Cons (AtomData (AtomID "oil")) AtomType))
          )
        `shouldBe` Right (Pair AtomType AtomType)
