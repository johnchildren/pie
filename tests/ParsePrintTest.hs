module ParsePrintTest
  ( hprop_parse_print_trip
  , hprop_to_from_core_trip
  )
where

import           Prelude
import           Data.List.NonEmpty                       ( NonEmpty(..) )
import           Data.Text                                ( Text )
import           Hedgehog                          hiding ( Var )
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Language.Pie.Symbols                     ( Symbol(..)
                                                          , VarName(..)
                                                          )
import           Language.Pie.Parse                       ( parsePie
                                                          , reservedWords
                                                          )
import           Language.Pie.Print                       ( printPie )
import           Language.Pie.Expr                        ( Expr(..)
                                                          , fromCore
                                                          , toCore
                                                          )


genVarName :: Gen VarName
genVarName = Gen.choice $ (<$> varNameText) <$> [flip VarName 0] -- TODO: support Dimmed?
 where
  varNameText :: Gen Text
  varNameText =
    Gen.filter (\x -> not $ x `elem` reservedWords) (Gen.text (Range.constant 1 15) Gen.alpha)


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
  , Int <$> Gen.integral (Range.constant 0 100)
  ]
  [ Gen.subterm2 genExprs genExprs The
  , Gen.subterm2 genExprs genExprs Cons
  , Gen.subterm2 genExprs genExprs Pair
  , Gen.subterm genExprs Car
  , Gen.subterm genExprs Cdr
--  , Gen.subtermM2 genExprs genExprs (\x y -> Pi <$> pure x <*> pure y)
--  , Gen.subtermM2 genExprs genExprs (\x y -> Sigma <$> pure x <*> pure y)
  , Gen.subterm2 genExprs genExprs Arrow
  , Gen.subtermM genExprs (\x -> Lambda <$> ((:| []) <$> genVarName) <*> pure x)
  , Gen.subtermM2
    genExprs
    genExprs
    (\x y -> App <$> pure x <*> Gen.nonEmpty (Range.constant 1 5) (pure y))
  , Gen.subterm genExprs Add1
  , Gen.subterm3 genExprs genExprs genExprs WhichNat
  , Gen.subterm3 genExprs genExprs genExprs IterNat
  , Gen.subterm3 genExprs genExprs genExprs RecNat
  ]

hprop_parse_print_trip :: Property
hprop_parse_print_trip = withTests 1000 . property $ do
  exprs <- forAll genExprs
  tripping exprs printPie parsePie

hprop_to_from_core_trip :: Property
hprop_to_from_core_trip = withTests 1000 . property $ do
  exprs <- forAll genExprs
  toCore exprs === toCore (fromCore (toCore exprs))
