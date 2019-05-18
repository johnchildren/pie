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
import qualified Language.Pie.Environment      as Env
import           Language.Pie.TypeChecker                 ( synth
                                                          , Binding(..)
                                                          , ctxToEnvironment
                                                          , TypeError
                                                          )

data InterpError = Parse PieParseError
                 | Eval EvalError
                 | Infer TypeError
                 deriving (Show)

type IState = Env.Env Binding
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

eval :: Env.Env Binding -> CoreExpr -> Interpreter Value
eval ctx expr = case val (ctxToEnvironment ctx) expr of
  Left  err -> throwE (Eval err)
  Right v   -> pure v

printError :: InterpError -> Interpreter ()
printError (Parse err) = liftIO . putStrLn $ errorBundlePretty err
printError (Eval  err) = liftIO $ print err
printError (Infer err) = liftIO $ print err

run :: Text -> Interpreter ()
run input = catchE run' printError
 where
  run' = do
    ctx  <- get
    stmt <- parse input
    case stmt of
      (Define v e) -> do
        (tOut, eOut) <- infer ctx (toCore e)
        eVal         <- eval ctx eOut
        tVal         <- eval ctx tOut
        put $ Env.insert v (Definition { _type = tVal, _value = eVal }) ctx
      (RawExpr e) -> do
        (tOut, eOut) <- infer ctx (toCore e)
        liftIO . Text.putStrLn . printPie $ fromCore (CThe tOut eOut)
