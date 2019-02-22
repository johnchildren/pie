{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Data.Functor.Foldable                    ( Fix(..) )
import           Language.Pie.Parse                       ( parsePie )
import           Language.Pie.Expr                        ( AtomID(..)
                                                          , VarName(..)
                                                          , Expr(..)
                                                          )

main :: IO ()
main = hspec $ do
  describe "Parse" $ do
    it "can parse the type Atom" $ do
      parsePie "Atom" `shouldBe` Right AtomType
