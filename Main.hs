{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Main
  ( main
  )
where

import           Control.Monad.Trans.State.Strict         ( evalStateT )
import           Control.Monad.Trans.Class                ( lift )
import           Control.Monad.Trans.Except               ( runExceptT
                                                          , ExceptT(..)
                                                          )
import           Control.Monad.IO.Class                   ( liftIO )
import           System.Console.Repline
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           System.Exit                              ( exitSuccess )
import           System.Console.Haskeline.MonadException  ( MonadException
                                                          , controlIO
                                                          , RunIO(..)
                                                          )
import           Language.Pie.Interpreter                 ( Interpreter
                                                          , run
                                                          )
import qualified Language.Pie.Environment      as Env



instance (MonadException m) => MonadException (ExceptT e m) where
  controlIO f = ExceptT $ controlIO $ \(RunIO run) ->
    let run' = RunIO (fmap ExceptT . run . runExceptT)
    in  fmap runExceptT $ f run'

type Repl a = HaskelineT Interpreter a

cmd :: String -> Repl ()
cmd s = lift $ run (Text.pack s)

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
        , "List"
        , "nil"
        , "::"
        , "Universe"
        ]
  return $ Text.unpack <$> filter (Text.isPrefixOf (Text.pack n)) keywords

options :: [(String, [String] -> Repl ())]
options = [("quit", quit)]

quit :: [String] -> Repl ()
quit _ = liftIO exitSuccess

ini :: Repl ()
ini = liftIO
  $ Text.putStrLn "Welcome to pie! Each line will be evaluated as an expr!"

main :: IO ()
main =
  (runExceptT $ flip evalStateT Env.empty $ evalRepl (pure "Pie> ")
                                                     cmd
                                                     options
                                                     (Just ':')
                                                     (Word completer)
                                                     ini
    )
    >>= \s -> case s of
          Left  err -> print err
          Right ()  -> pure ()
