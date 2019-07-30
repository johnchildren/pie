{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Pie.Expr
  ( Expr(..)
  , ExprF(..)
  , Clos(..)
  , CoreExpr(..)
  , CoreExprF(..)
  , toCore
  , fromCore
  )
where

import           Prelude                                  ( Show
                                                          , Eq
                                                          , Integer
                                                          , ($)
                                                          , (>)
                                                          , (+)
                                                          , (-)
                                                          , error
                                                          )
import           Data.List.NonEmpty                       ( NonEmpty((:|))
                                                          , (<|)
                                                          )
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Functor.Foldable                    ( Base
                                                          , cata
                                                          )
import           Data.Functor.Foldable.TH                 ( makeBaseFunctor )
import           Language.Pie.Symbols                     ( Symbol
                                                          , VarName
                                                            ( VarName
                                                            , Dimmed
                                                            )
                                                          )

data Expr = The Expr Expr
         | Var VarName
         | Atom
         | Quote Symbol
         | Pair Expr Expr
         | Sigma (NonEmpty (VarName, Expr)) Expr
         | Cons Expr Expr
         | Car Expr
         | Cdr Expr
         | Arrow Expr Expr
         | Pi (NonEmpty (VarName, Expr)) Expr
         | Lambda (NonEmpty VarName) Expr
         | App Expr (NonEmpty Expr)
         | Nat
         | Zero
         | Add1 Expr
         | Int Integer
         | WhichNat Expr Expr Expr
         | IterNat Expr Expr Expr
         | RecNat Expr Expr Expr
         | List Expr
         | Nil
         | ListExp Expr Expr
         | Universe
        deriving (Show, Eq)

makeBaseFunctor ''Expr

-- Closure indiciates this value shouldn't be computed immediately.
newtype Clos = Clos CoreExpr
  deriving (Show, Eq)

-- | Core expression
data CoreExpr = CThe CoreExpr CoreExpr
         | CVar VarName
         | CAtom
         | CQuote Symbol
         | CSigma VarName CoreExpr Clos
         | CCons CoreExpr CoreExpr
         | CCar CoreExpr
         | CCdr CoreExpr
         | CPi VarName CoreExpr Clos
         | CLambda VarName Clos
         | CApp CoreExpr CoreExpr
         | CNat
         | CZero
         | CAdd1 CoreExpr
         | CWhichNat CoreExpr CoreExpr Clos
         | CIterNat CoreExpr CoreExpr Clos
         | CRecNat CoreExpr CoreExpr Clos
         | CList CoreExpr
         | CNil
         | CListExp CoreExpr CoreExpr
         | CUniverse
        deriving (Show, Eq)

makeBaseFunctor ''CoreExpr

type Algebra t a = Base t a -> a

toCore :: Expr -> CoreExpr
toCore = cata toCore'
 where
  toCore' :: Algebra Expr CoreExpr
  toCore' (TheF e1 e2)                 = CThe e1 e2
  toCore' (VarF v    )                 = CVar v
  toCore' AtomF                        = CAtom
  toCore' (QuoteF s       )            = CQuote s
  -- | ΣF-Pair
  toCore' (PairF  e1 e2   )            = CSigma (Dimmed "x" 0) e1 (Clos e2)
  toCore' (SigmaF vs body )            = encodeSigma vs body
  toCore' (ConsF  e1 e2   )            = CCons e1 e2
  toCore' (CarF pr        )            = CCar pr
  toCore' (CdrF pr        )            = CCdr pr
  -- | FunF-→1
  toCore' (ArrowF  e1 e2  )            = CPi (Dimmed "x" 0) e1 (Clos e2)
  toCore' (PiF     vs body)            = encodePi vs body
  toCore' (LambdaF vs body)            = encodeLambda vs body
  toCore' (AppF    f  args)            = encodeApp f args
  toCore' NatF                         = CNat
  toCore' ZeroF                        = CZero
  toCore' (Add1F n                   ) = CAdd1 n
  toCore' (IntF  n                   ) = encodeInteger n
  toCore' (WhichNatF target base step) = CWhichNat target base (Clos step)
  toCore' (IterNatF  target base step) = CIterNat target base (Clos step)
  toCore' (RecNatF   target base step) = CRecNat target base (Clos step)
  toCore' (ListF e                   ) = CList e
  toCore' NilF                         = CNil
  toCore' (ListExpF e1 e2)             = CListExp e1 e2
  toCore' UniverseF                    = CUniverse

encodeSigma :: NonEmpty (VarName, CoreExpr) -> CoreExpr -> CoreExpr
encodeSigma ((v, ty) :| []) body = CSigma v ty (Clos body)
encodeSigma ((v, ty) :| vs) body =
  CSigma v ty (Clos (encodeSigma (NonEmpty.fromList vs) body))

encodePi :: NonEmpty (VarName, CoreExpr) -> CoreExpr -> CoreExpr
encodePi ((v, ty) :| []) body = CPi v ty (Clos body)
encodePi ((v, ty) :| vs) body =
  CPi v ty (Clos (encodePi (NonEmpty.fromList vs) body))

encodeLambda :: NonEmpty VarName -> CoreExpr -> CoreExpr
encodeLambda (v :| []) body = CLambda v (Clos body)
encodeLambda (v :| vs) body =
  CLambda v (Clos (encodeLambda (NonEmpty.fromList vs) body))

encodeApp :: CoreExpr -> NonEmpty CoreExpr -> CoreExpr
encodeApp e1 (e2 :| []) = CApp e1 e2
encodeApp e1 (e2 :| es) = CApp e1 (encodeApp e2 (NonEmpty.fromList es))

encodeInteger :: Integer -> CoreExpr
encodeInteger 0         = CZero
encodeInteger n | n > 0 = CAdd1 $ encodeInteger (n - 1)
encodeInteger _         = error "attempted to encode a negative integer"

fromCore :: CoreExpr -> Expr
fromCore = cata fromCore'
 where
  fromCore' :: Algebra CoreExpr Expr
  fromCore' (CTheF e1 e2)                       = The e1 e2
  fromCore' (CVarF v    )                       = Var v
  fromCore' CAtomF                              = Atom
  fromCore' (CQuoteF s                        ) = Quote s
  fromCore' (CSigmaF (Dimmed _ _) e1 (Clos e2)) = Pair e1 (fromCore e2)
  fromCore' (CSigmaF v@(VarName _ _) e1 (Clos (fromCore -> Sigma vs e2))) =
    Sigma ((v, e1) <| vs) e2
  fromCore' (CSigmaF v@(VarName _ _) e1 (Clos (fromCore -> e2))) =
    Sigma ((v, e1) :| []) e2
  fromCore' (CConsF e1 e2                  ) = Cons e1 e2
  fromCore' (CCarF pr                      ) = Car pr
  fromCore' (CCdrF pr                      ) = Cdr pr
  fromCore' (CPiF (Dimmed _ _) e1 (Clos e2)) = Arrow e1 (fromCore e2)
  fromCore' (CPiF v@(VarName _ _) e1 (Clos (fromCore -> Pi vs e2))) =
    Pi ((v, e1) <| vs) e2
  fromCore' (CPiF v@(VarName _ _) e1 (Clos (fromCore -> e2))) =
    Pi ((v, e1) :| []) e2
  fromCore' (CLambdaF v (Clos (fromCore -> Lambda vs e))) = Lambda (v <| vs) e
  fromCore' (CLambdaF v (Clos (fromCore -> e))) = Lambda (v :| []) e
  fromCore' (CAppF e1 (App e2 es)) = App e1 (e2 <| es)
  fromCore' (CAppF e1 e2)    = App e1 (e2 :| [])
  fromCore' CNatF            = Nat
--  fromCore' CZeroF                               = Zero
  fromCore' CZeroF           = Int 0
  fromCore' (CAdd1F (Int n)) = Int (n + 1)
  fromCore' (CAdd1F n      ) = Add1 n
  fromCore' (CWhichNatF target base (Clos step)) =
    WhichNat target base (fromCore step)
  fromCore' (CIterNatF target base (Clos step)) =
    IterNat target base (fromCore step)
  fromCore' (CRecNatF target base (Clos step)) =
    RecNat target base (fromCore step)
  fromCore' (CListF e)        = List e
  fromCore' CNilF             = Nil
  fromCore' (CListExpF e1 e2) = ListExp e1 e2
  fromCore' CUniverseF        = Universe
