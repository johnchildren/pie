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
                                                          , Cmd
                                                          , Options
                                                          , CompleterStyle(..)
                                                          , WordCompleter
                                                          , evalRepl
                                                          )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           System.Exit                              ( exitSuccess )
import           Language.Pie.Print                       ( printPie )
import           Language.Pie.Parse                       ( parsePie
                                                          , errorBundlePretty
                                                          )
import           Language.Pie.Expr                        ( toCore
                                                          , fromCore
                                                          )
import qualified Language.Pie.Environment      as Env
import           Language.Pie.TypeChecker                 ( synth )




type Repl a = HaskelineT IO a

cmd :: (MonadIO m) => Command (HaskelineT m)
cmd input = liftIO $ case parsePie (Text.pack input) of
  Right expr -> case synth Env.empty (toCore expr) of
    Right typedExpr -> Text.putStrLn . printPie $ fromCore typedExpr
    Left  err       -> print err
  Left err -> putStrLn $ errorBundlePretty err

completer :: Monad m => WordCompleter m
completer n = do
  let keywords =
        [ "The"
        , "Atom"
        , "Pair"
        , "Sigma"
        , "cons"
        , "car"
        , "cdr"
        , "->"
        , "Pi"
        , "lambda"
        , "Nat"
        , "zero"
        , "add1"
        , "which-Nat"
        , "iter-Nat"
        , "rec-Nat"
        , "Universe"
        ]
  return $ Text.unpack <$> filter (Text.isPrefixOf (Text.pack n)) keywords

options :: (MonadIO m) => Options (HaskelineT m)
options = [("quit", quit)]

quit :: (MonadIO m) => Cmd (HaskelineT m)
quit _ = liftIO exitSuccess

ini :: Repl ()
ini = liftIO
  $ Text.putStrLn "Welcome to pie! Each line will be evaluated as an expr!"

main :: IO ()
main = evalRepl (pure "Pie> ") cmd options (Just ':') (Word completer) ini
