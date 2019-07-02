{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  )
where

import           Prelude                           hiding ( putStrLn )
import           Control.Effect                           ( Carrier
                                                          , Member
                                                          )
import           Control.Effect.Lift                      ( Lift
                                                          , LiftC
                                                          , sendM
                                                          , runM
                                                          )
import           Control.Effect.Error                     ( Error
                                                          , ErrorC
                                                          , runError
                                                          )
import           Control.Effect.State.Strict              ( State
                                                          , StateC
                                                          , evalState
                                                          )
import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Language.Pie.Interpreter                 ( run
                                                          , InterpError
                                                          , IState
                                                          )
import qualified Language.Pie.Environment      as Env



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
    line <- sendM Text.getLine
    run line
    loop
