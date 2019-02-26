module Main
  ( main
  )
where

import           Language.Pie.Judgement                   ( judgement2 )
import           Language.Pie.Expr                        ( AtomID(..)
                                                          , Expr(..)
                                                          )


main :: IO ()
main = do
  let foo = AtomData (AtomID "foo")
  print $ judgement2 foo AtomType foo
