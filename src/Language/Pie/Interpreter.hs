{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Pie.Interpreter
  ( run
  , IState
  , InterpError(..)
  )
where

import           Prelude                           hiding ( putStrLn
                                                          , print
                                                          )
import           Control.Effect                           ( Carrier
                                                          , Member
                                                          )
import           Control.Effect.Error                     ( throwError
                                                          , catchError
                                                          , Error
                                                          )
import           Control.Effect.State.Strict              ( get
                                                          , put
                                                          , State
                                                          )
import           Control.Effect.Lift                      ( sendM
                                                          , Lift
                                                          )
import           Data.Text                                ( Text )
import qualified Data.Text.IO                  as Text
import qualified Data.Text                     as Text
import           Language.Pie.Print                       ( printPie )
import           Language.Pie.Eval                        ( val
                                                          , EvalError
                                                          )
import           Language.Pie.Parse                       ( parsePieStatement
                                                          , errorBundlePretty
                                                          , Statement(..)
                                                          , PieParseError
                                                          )
import           Language.Pie.Expr                        ( toCore
                                                          , fromCore
                                                          , CoreExpr(..)
                                                          )
import           Language.Pie.Values                      ( Value )
import           Language.Pie.Environment                 ( Env )
import qualified Language.Pie.Environment      as Env
import           Language.Pie.Symbols                     ( VarName )
import           Language.Pie.TypeChecker                 ( synth
                                                          , check
                                                          , Binding(..)
                                                          , ctxToEnvironment
                                                          , TypeError
                                                          )


type IState = (Env Binding, Env Claimed)

data InterpError = Parse PieParseError
                 | Eval EvalError
                 | Infer TypeError
                 | Check TypeError
                 | NonTypeClaim
                 | NoClaim VarName
                 deriving (Show)

newtype Claimed = Claimed Value

printError :: (Member (Lift IO) sig, Carrier sig m) => InterpError -> m ()
printError (Parse err) =
  sendM . Text.putStrLn . Text.pack $ errorBundlePretty err
printError (Eval  err)  = sendM . Text.putStrLn . Text.pack . show $ err
printError (Infer err)  = sendM . Text.putStrLn . Text.pack . show $ err
printError (Check err)  = sendM . Text.putStrLn . Text.pack . show $ err
printError NonTypeClaim = sendM $ Text.putStrLn "Not a type"
printError (NoClaim var) =
  sendM . Text.putStrLn $ "No claim: " <> Text.pack (show var)

parse :: (Member (Error InterpError) sig, Carrier sig m) => Text -> m Statement
parse input = case parsePieStatement input of
  Left  err  -> throwError (Parse err)
  Right expr -> pure expr

infer
  :: (Member (Error InterpError) sig, Carrier sig m)
  => Env Binding
  -> CoreExpr
  -> m (CoreExpr, CoreExpr)
infer ctx expr = case synth ctx expr of
  Left  err  -> throwError (Infer err)
  Right pair -> pure pair

checkClaim
  :: (Member (Error InterpError) sig, Carrier sig m)
  => Env Binding
  -> CoreExpr
  -> Value
  -> m CoreExpr
checkClaim ctx expr claim = case check ctx expr claim of
  Left  err  -> throwError (Check err)
  Right pair -> pure pair

eval
  :: (Member (Error InterpError) sig, Carrier sig m)
  => Env Binding
  -> CoreExpr
  -> m Value
eval ctx expr = case val (ctxToEnvironment ctx) expr of
  Left  err -> throwError (Eval err)
  Right v   -> pure v

run
  :: ( Member (Error InterpError) sig
     , Member (State IState) sig
     , Member (Lift IO) sig
     , Carrier sig m
     )
  => Text
  -> m ()
run input = catchError (run' input) printError
 where
  run' input' = do
    (ctx, claims) <- get
    stmt          <- parse input'
    case stmt of
      (Claim v e) -> do
        (tOut, eOut) <- infer ctx (toCore e)
        case tOut of
          CUniverse -> do
            eVal <- eval ctx eOut
            let newClaims = Env.insert v (Claimed eVal) claims
            put (ctx, newClaims)
          _ -> throwError NonTypeClaim
      (Define v e) -> case Env.lookup v claims of
        Nothing             -> throwError $ NoClaim v
        Just (Claimed tVal) -> do
          eOut <- checkClaim ctx (toCore e) tVal
          eVal <- eval ctx eOut
          let newCtx =
                Env.insert v (Definition { _type = tVal, _value = eVal }) ctx
          put (newCtx, claims)
      (RawExpr e) -> do
        (tOut, eOut) <- infer ctx (toCore e)
        sendM . Text.putStrLn . printPie $ fromCore (CThe tOut eOut)
