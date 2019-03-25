module ParsePrintTest
  ( hprop_parse_print_trip
  )
where

import           Hedgehog                          hiding ( Var )
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Language.Pie.Symbols                     ( Symbol(..)
                                                          , VarName(..)
                                                          )
import           Language.Pie.Parse                       ( parsePie )
import           Language.Pie.Print                       ( printPie )
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


hprop_parse_print_trip :: Property
hprop_parse_print_trip = withTests 1000 . property $ do
  exprs <- forAll genExprs
  tripping exprs printPie parsePie
