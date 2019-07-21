{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  )
where

import           Prelude                           hiding ( putStrLn )
import           Control.Effect.Lift                      ( LiftC
                                                          , sendM
                                                          , runM
                                                          )
import           Control.Effect.Error                     ( ErrorC
                                                          , runError
                                                          , catchError
                                                          )
import           Control.Effect.State.Strict              ( StateC
                                                          , evalState
                                                          )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Language.Pie.Interpreter                 ( interp
                                                          , printError
                                                          , InterpError
                                                          , IState
                                                          )
import qualified Language.Pie.Environment      as Env
import           System.IO                                ( hFlush
                                                          , stdout
                                                          )


main :: IO ()
main = do
  Text.putStrLn "Welcome to pie! Each line will be evaluated as an expr!"
  res <- runM $ runError $ evalState @IState (Env.empty, Env.empty) loop
  case (res :: Either InterpError ()) of
    Left err -> Text.putStrLn $ "uncaught exception: " <> Text.pack
      (show @InterpError err)
    Right _ -> pure ()
 where
  loop :: StateC IState (ErrorC InterpError (LiftC IO)) ()
  loop = do
    sendM $ putStr "pie> "
    sendM $ hFlush stdout
    line <- sendM Text.getLine
    catchError (interp line >>= sendM . Text.putStrLn) printError
    loop
