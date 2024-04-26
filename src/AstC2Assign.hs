module AstC2Assign (Assign (..)) where

import Expr (Expr)
import Var (Var)

data Assign = Assign
  { lhs :: Var,
    rhs :: Expr
  }
  deriving (Show, Eq, Ord)