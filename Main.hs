{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where


import           System.IO                                ( hFlush
                                                          , stdout
                                                          )
import qualified Data.Text.IO                  as Text
import           Language.Pie.Parse                       ( parsePie
                                                          , parseErrorPretty
                                                          )
import           Language.Pie.Print                       ( printPie )
import           Language.Pie.Eval                        ( emptyEnv
                                                          , evalPie
                                                          )



main :: IO ()
main = do
  putStrLn "Welcome to pie! Each line will be evaluated as an expr!"
  go
 where
  go :: IO ()
  go = do
    Text.putStr "pie> "
    hFlush stdout
    line <- Text.getLine
    case parsePie line of
      Right expr -> case evalPie emptyEnv expr of
        Right evald -> Text.putStrLn (printPie evald)
        Left  err   -> print err
      Left err -> putStrLn $ parseErrorPretty err
    putStrLn ""
    go
