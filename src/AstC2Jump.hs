module AstC2Jump (Jump (..)) where

import Var (Var)

data Jump a = Jump
  { target :: a,
    condition :: Var
  }
  deriving (Show, Eq, Ord, Functor)