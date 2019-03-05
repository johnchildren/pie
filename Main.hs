module Main
  ( main
  )
where


import           System.IO                                ( hFlush
                                                          , stdout
                                                          )
import           Language.Pie.Parse                       ( parsePie )
import           Language.Pie.Print                       ( printPie )
import           Language.Pie.Eval                        ( emptyEnv
                                                          , evalPie
                                                          )



main :: IO ()
main = do
  putStrLn "Welcome to lazyfuck! Each line will be evaluated as a script!"
  go
 where
  go :: IO ()
  go = do
    putStr "pie)) "
    hFlush stdout
    line <- getLine
    case parsePie line of
      Right expr -> case evalPie emptyEnv expr of
        Right evald -> putStrLn (printPie evald)
        Left  err   -> print err
      Left err -> print err
    putStrLn ""
    go
