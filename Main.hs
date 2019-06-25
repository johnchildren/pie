{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  )
where

import           Prelude
import           Control.Effect                           ( Carrier
                                                          , Member
                                                          )
import           Control.Effect.Lift                      ( sendM
                                                          , Lift
                                                          , runM
                                                          )
import           Control.Effect.Error                     ( Error
                                                          , runError
                                                          )
import           Control.Effect.State.Strict              ( State
                                                          , evalState
                                                          )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Language.Pie.Interpreter                 ( run
                                                          , InterpError
                                                          , IState
                                                          )
import qualified Language.Pie.Environment      as Env




ini :: (Member (Lift IO) sig, Carrier sig m) => m ()
ini = sendM
  $ Text.putStrLn "Welcome to pie! Each line will be evaluated as an expr!"

main :: IO ()
main = do
  res <- runM $ runError $ evalState @IState (Env.empty, Env.empty)
                                             (ini >> loop)
  case res of
    Left err -> Text.putStrLn $ "uncaught exception: " <> Text.pack
      (show @InterpError err)
    Right _ -> pure ()
 where
  loop
    :: ( Member (Error InterpError) sig
       , Member (State IState) sig
       , Member (Lift IO) sig
       , Carrier sig m
       )
    => m ()
  loop = do
    line <- sendM Text.getLine
    run line
    loop
