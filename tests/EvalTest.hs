module EvalTest
  ( test_eval
  )
where

import           Prelude
import           Data.Text                                ( Text )
import           Test.Tasty
import           Test.Tasty.HUnit
import           Language.Pie.Symbols                     ( Symbol(..)
                                                          , VarName(..)
                                                          )
import qualified Language.Pie.Environment      as Env
import           Language.Pie.Eval                        ( val
                                                          , EvalError
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

test_eval :: TestTree
test_eval = testGroup
  "Evaluation Tests"
  [ testGroup
    "normalisation"
    [ testCase "normalises expressions"
    $   evalExpr
          (Car
            (Cons (Cons (mkAtom "aubergine") (mkAtom "courgette"))
                  (mkAtom "tomato")
            )
          )
    @=? Right (VCons (mkAtomVal "aubergine") (mkAtomVal "courgette"))
    , testCase "normalises expression of types and values"
    $   evalExpr
          (Pair (Car (Cons Atom (mkAtom "olive")))
                (Cdr (Cons (mkAtom "oil") Atom))
          )
    @=? Right
          (VSigma
            VAtom
            (CLOS Env.empty
                  (Dimmed "x")
                  (CCdr (CCons (mkCoreAtom "oil") CAtom))
            )
          )
    ]
  , testGroup
    "lambda expressions"
    [ testCase "can apply lambda expressions"
    $   evalExpr (App (mkLambda "x" (mkVar "x")) Atom)
    @=? Right VAtom
    , testCase "will normalise while applying a lambda expression"
    $ evalExpr (App (mkLambda "x" (Car (Cons (mkVar "x") (mkAtom "foo")))) Atom)
    @=? Right VAtom
    , testCase "will ignore unused variables"
    $   evalExpr (App (mkLambda "x" (mkAtom "foo")) Atom)
    @=? Right (mkAtomVal "foo")
    , testCase "works with nested lambda applications"
    $   evalExpr
          (App (App (mkLambda "x" (mkLambda "y" (mkVar "x"))) Atom) (mkAtom "foo")
          )
    @=? Right VAtom
    ]
  , testGroup
    "which-Nat"
    [ testCase "evaluates to base when target is zero"
    $ evalExpr (WhichNat Zero (mkAtom "naught") (mkLambda "n" (mkAtom "more")))
    @=? Right (mkAtomVal "naught")
    , testCase "evaluates to step n when target is (add1 n)"
    $   evalExpr
          (WhichNat (Add1 (Add1 (Add1 (Add1 Zero))))
                    (mkAtom "naught")
                    (mkLambda "n" (mkAtom "more"))
          )
    @=? Right (mkAtomVal "more")
    ]
  , testGroup
    "iter-Nat"
    [ testCase "evaluates to base when target is zero"
    $   evalExpr (IterNat Zero (mkAtom "naught") (mkLambda "n" (mkAtom "more")))
    @=? Right (mkAtomVal "naught")
    , testCase "evaluates to step n when target is (add1 n)"
    $   evalExpr
          (IterNat (Add1 (Add1 (Add1 (Add1 Zero))))
                   (mkAtom "naught")
                   (mkLambda "n" (mkAtom "more"))
          )
    @=? Right (mkAtomVal "more")
    , testCase "each add1 in the value of target is replaced by a step"
    $   evalExpr
          (IterNat (Add1 (Add1 (Add1 (Add1 (Add1 Zero)))))
                   (Add1 (Add1 (Add1 Zero)))
                   (mkLambda "smaller" (Add1 (mkVar "smaller")))
          )
    @=? Right
          (VAdd1 (VAdd1 (VAdd1 (VAdd1 (VAdd1 (VAdd1 (VAdd1 (VAdd1 VZero)))))))
          )
    ]
  , testGroup
    "rec-Nat"
    [ testCase "evaluates to base when target is zero"
    $   evalExpr
          (RecNat Zero
                  (mkAtom "naught")
                  (mkLambda "n" (mkLambda "i" (mkAtom "more")))
          )
    @=? Right (mkAtomVal "naught")
    , testCase
      "evaluates to (step n (iter-Nat n base step)) when target is (add1 n)"
    $   evalExpr
          (RecNat (Add1 (Add1 (Add1 (Add1 Zero))))
                  (mkAtom "naught")
                  (mkLambda "n" (mkLambda "i" (mkAtom "more")))
          )
    @=? Right (mkAtomVal "more")
    ]
  ]

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
