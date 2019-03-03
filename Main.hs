module Main
  ( main
  )
where

import           Language.Pie.Eval                        ( emptyEnv )
import           Language.Pie.Judgement                   ( judgement2 )
import           Language.Pie.Expr                        ( AtomID(..)
                                                          , Expr(..)
                                                          )


main :: IO ()
main = do
  let foo = AtomData (AtomID "foo")
  print $ judgement2 emptyEnv foo AtomType foo
