module AstC2Jump (Jump (..)) where

import AstC2Expr (Expr)

data Jump label = Jump
  { target :: label,
    condition :: Expr
  }
  deriving (Show, Eq, Ord, Functor)