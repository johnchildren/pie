{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Pie.Interpreter
  ( interp
  , printError
  , IState
  , InterpError(..)
  )
where

import           Prelude                           hiding ( putStrLn
                                                          , print
                                                          )
import           Control.Effect                           ( Carrier
                                                          , Member
                                                          , Effect
                                                          , run
                                                          )
import           Control.Effect.Error                     ( throwError
                                                          , runError
                                                          , Error
                                                          )
import           Control.Effect.Fresh                     ( runFresh )
import           Control.Effect.Reader                    ( Reader
                                                          , ask
                                                          , runReader
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
import           Language.Pie.Values                      ( Value
                                                          , Normal(..)
                                                          )
import           Language.Pie.Environment                 ( Env )
import qualified Language.Pie.Environment      as Env
import           Language.Pie.Symbols                     ( VarName )
import           Language.Pie.TypeChecker                 ( synth
                                                          , check
                                                          , Binding(..)
                                                          , ctxToEnvironment
                                                          , readBackNorm
                                                          , TypeError
                                                          )


type IState = (Env Binding, Env Claimed)

data InterpError = Parse PieParseError
                 | Eval EvalError
                 | Infer TypeError
                 | Check TypeError
                 | ReadBack TypeError
                 | NonTypeClaim
                 | NoClaim VarName
                 deriving (Show, Eq)

newtype Claimed = Claimed Value

printError :: (Member (Lift IO) sig, Carrier sig m) => InterpError -> m ()
printError (Parse err) =
  sendM . Text.putStrLn . Text.pack $ errorBundlePretty err
printError (Eval     err) = sendM . Text.putStrLn . Text.pack . show $ err
printError (Infer    err) = sendM . Text.putStrLn . Text.pack . show $ err
printError (Check    err) = sendM . Text.putStrLn . Text.pack . show $ err
printError (ReadBack err) = sendM . Text.putStrLn . Text.pack . show $ err
printError NonTypeClaim   = sendM $ Text.putStrLn "Not a type"
printError (NoClaim var) =
  sendM . Text.putStrLn $ "No claim: " <> Text.pack (show var)

parse :: (Member (Error InterpError) sig, Carrier sig m) => Text -> m Statement
parse input = case parsePieStatement input of
  Left  err  -> throwError (Parse err)
  Right expr -> pure expr

infer
  :: ( Member (Error InterpError) sig
     , Member (Reader (Env Binding)) sig
     , Effect sig
     , Carrier sig m
     )
  => CoreExpr
  -> m (CoreExpr, CoreExpr)
infer expr = do
  res <- runFresh . runError $ synth expr
  case res of
    Left  err  -> throwError (Check err)
    Right pair -> pure pair

checkClaim
  :: ( Member (Error InterpError) sig
     , Member (Reader (Env Binding)) sig
     , Effect sig
     , Carrier sig m
     )
  => CoreExpr
  -> Value
  -> m CoreExpr
checkClaim expr claim = do
  res <- runFresh . runError $ check expr claim
  case res of
    Left  err  -> throwError (Check err)
    Right pair -> pure pair

eval
  :: ( Member (Error InterpError) sig
     , Member (Reader (Env Binding)) sig
     , Carrier sig m
     )
  => CoreExpr
  -> m Value
eval expr = do
  gamma <- ask
  case run . runError . runReader (ctxToEnvironment gamma) $ val expr of
    Left  err -> throwError (Eval err)
    Right v   -> pure v

readBack
  :: ( Member (Error InterpError) sig
     , Member (Reader (Env Binding)) sig
     , Effect sig
     , Carrier sig m
     )
  => Value
  -> Value
  -> m CoreExpr
readBack ty value = do
  res <- runFresh . runError $ readBackNorm (NormThe ty value)
  case res of
    Left  err -> throwError (ReadBack err)
    Right v   -> pure v

interp
  :: ( Member (Error InterpError) sig
     , Member (State IState) sig
     , Effect sig
     , Carrier sig m
     )
  => Text
  -> m Text
interp input = do
  (gamma, claims) <- get
  stmt            <- parse input
  runReader gamma $ case stmt of
    (Claim v e) -> do
      (tOut, eOut) <- infer (toCore e)
      case tOut of
        CUniverse -> do
          eVal <- eval eOut
          let newClaims = Env.insert v (Claimed eVal) claims
          put (gamma, newClaims)
          pure "claimed"
        _ -> throwError NonTypeClaim
    (Define v e) -> case Env.lookup v claims of
      Nothing             -> throwError $ NoClaim v
      Just (Claimed tVal) -> do
        eOut <- checkClaim (toCore e) tVal
        eVal <- eval eOut
        let newCtx =
              Env.insert v (Definition { _type = tVal, _value = eVal }) gamma
        put (newCtx, claims)
        pure "defined"
    (RawExpr e) -> do
      (tOut, eOut) <- infer (toCore e)
      tOutVal      <- eval tOut
      eOutVal      <- eval eOut
      eOutNormed   <- readBack tOutVal eOutVal
      pure . printPie $ fromCore (CThe tOut eOutNormed)
