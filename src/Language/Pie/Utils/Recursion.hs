module Language.Pie.Utils.Recursion
  ( Term(..)
  , Algebra
  , cata
  )
where

import           Control.Category               ( (>>>) )

newtype Term f = In { out :: f (Term f) }

type Algebra f a = f a -> a

cata :: (Functor f) => Algebra f a -> Term f -> a
cata f = out >>> fmap (cata f) >>> f
