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
import           Language.Pie.Print                       ( printPie )
import           Language.Pie.Eval                        ( emptyEnv
                                                          , evalPie
                                                          )




type Repl a = HaskelineT IO a

cmd :: (MonadIO m) => Command (HaskelineT m)
cmd input = liftIO $ case parsePie (Text.pack input) of
  Right expr -> case evalPie emptyEnv expr of
    Right evald -> Text.putStrLn (printPie evald)
    Left  err   -> print err
  Left err -> putStrLn $ parseErrorPretty err

completer :: Monad m => WordCompleter m
completer _ = pure []

options :: Options (HaskelineT m)
options = []

ini :: Repl ()
ini = liftIO
  $ Text.putStrLn "Welcome to pie! Each line will be evaluated as an expr!"

main :: IO ()
main = evalRepl (pure "Pie> ") cmd options Nothing (Word completer) ini
