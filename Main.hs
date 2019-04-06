{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )
import           System.Console.Repline                   ( HaskelineT
                                                          , Command
                                                          , Options
                                                          , CompleterStyle(..)
                                                          , WordCompleter
                                                          , evalRepl
                                                          )
import           Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Language.Pie.Parse                       ( parsePie
                                                          , parseErrorPretty
                                                          )
import qualified Language.Pie.Environment      as Env
import           Language.Pie.TypeChecker                 ( tyInteract )




type Repl a = HaskelineT IO a

cmd :: (MonadIO m) => Command (HaskelineT m)
cmd input = liftIO $ case parsePie (Text.pack input) of
  Right expr -> tyInteract Env.empty expr
  Left  err  -> putStrLn $ parseErrorPretty err

completer :: Monad m => WordCompleter m
completer _ = pure []

options :: Options (HaskelineT m)
options = []

ini :: Repl ()
ini = liftIO
  $ Text.putStrLn "Welcome to pie! Each line will be evaluated as an expr!"

main :: IO ()
main = evalRepl (pure "Pie> ") cmd options Nothing (Word completer) ini
