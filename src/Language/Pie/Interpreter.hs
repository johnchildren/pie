module Language.Pie.Interpreter
  ( run
  , Interpreter
  , InterpError(..)
  )
where

import           Control.Monad.Trans.State.Strict         ( StateT )
import qualified Control.Monad.Trans.State.Strict
                                               as State
import           Control.Monad.Trans.Class                ( lift )
import           Control.Monad.Trans.Except               ( ExceptT )
import qualified Control.Monad.Trans.Except    as Except
import           Control.Monad.IO.Class                   ( liftIO )
import           Data.Text                                ( Text )
import qualified Data.Text.IO                  as Text
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

data InterpError = Parse PieParseError
                 | Eval EvalError
                 | Infer TypeError
                 | NonTypeClaim
                 | NoClaim VarName
                 deriving (Show)

newtype Claimed = Claimed Value

type IState = (Env Binding, Env Claimed)
type Interpreter = StateT IState (ExceptT InterpError IO)

get :: Interpreter IState
get = State.get

put :: IState -> Interpreter ()
put = State.put

throwE :: InterpError -> Interpreter a
throwE = lift . Except.throwE

catchE :: Interpreter a -> (InterpError -> Interpreter a) -> Interpreter a
catchE = State.liftCatch Except.catchE

parse :: Text -> Interpreter Statement
parse input = case parsePieStatement input of
  Left  err  -> throwE (Parse err)
  Right expr -> pure expr

infer :: Env.Env Binding -> CoreExpr -> Interpreter (CoreExpr, CoreExpr)
infer ctx expr = case synth ctx expr of
  Left  err  -> throwE (Infer err)
  Right pair -> pure pair

checkClaim :: Env.Env Binding -> CoreExpr -> Value -> Interpreter CoreExpr
checkClaim ctx expr claim = case check ctx expr claim of
  Left  err  -> throwE (Infer err)
  Right pair -> pure pair

eval :: Env.Env Binding -> CoreExpr -> Interpreter Value
eval ctx expr = case val (ctxToEnvironment ctx) expr of
  Left  err -> throwE (Eval err)
  Right v   -> pure v

printError :: InterpError -> Interpreter ()
printError (Parse err)  = liftIO . putStrLn $ errorBundlePretty err
printError (Eval  err)  = liftIO $ print err
printError (Infer err)  = liftIO $ print err
printError NonTypeClaim = liftIO . putStrLn $ "Not a type"
printError (NoClaim var) = liftIO . putStrLn $ "No claim: " <> show var

run :: Text -> Interpreter ()
run input = catchE run' printError
 where
  run' = do
    (ctx, claims) <- get
    stmt          <- parse input
    case stmt of
      (Claim v e) -> do
        (tOut, eOut) <- infer ctx (toCore e)
        case tOut of
          CUniverse -> do
            eVal <- eval ctx eOut
            let newClaims = Env.insert v (Claimed eVal) claims
            put (ctx, newClaims)
          _ -> throwE NonTypeClaim
      (Define v e) -> case Env.lookup v claims of
        Nothing          -> throwE $ NoClaim v
        Just (Claimed tVal) -> do
          eOut <- checkClaim ctx (toCore e) tVal
          eVal         <- eval ctx eOut
          let newCtx =
                Env.insert v (Definition { _type = tVal, _value = eVal }) ctx
          put (newCtx, claims)
      (RawExpr e) -> do
        (tOut, eOut) <- infer ctx (toCore e)
        liftIO . Text.putStrLn . printPie $ fromCore (CThe tOut eOut)
