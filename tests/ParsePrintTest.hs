module ParsePrintTest
  ( hprop_parse_print_trip
  )
where

import           Prelude
import           Hedgehog                          hiding ( Var )
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Language.Pie.Symbols                     ( Symbol(..)
                                                          , VarName(..)
                                                          )
import           Language.Pie.Parse                       ( parsePie )
import           Language.Pie.Print                       ( printPie )
import           Language.Pie.Expr                        ( Expr(..) )


genVarName :: Gen VarName
genVarName =
  Gen.choice
    $   (\x -> x <$> Gen.text (Range.constant 1 10) Gen.alpha)
    <$> [VarName] -- TODO: support Dimmed?

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
  , Gen.subtermM2 genExprs
                  genExprs
                  (\x y -> Pi <$> genVarName <*> pure x <*> pure y)
  , Gen.subtermM2 genExprs
                  genExprs
                  (\x y -> Sigma <$> genVarName <*> pure x <*> pure y)
  , Gen.subterm2 genExprs genExprs Arrow
  , Gen.subtermM genExprs (\x -> Lambda <$> genVarName <*> pure x)
  , Gen.subterm2 genExprs genExprs App
  , Gen.subterm genExprs Add1
  , Gen.subterm3 genExprs genExprs genExprs WhichNat
  , Gen.subterm3 genExprs genExprs genExprs IterNat
  , Gen.subterm3 genExprs genExprs genExprs RecNat
  ]


hprop_parse_print_trip :: Property
hprop_parse_print_trip = withTests 1000 . property $ do
  exprs <- forAll genExprs
  tripping exprs printPie parsePie
