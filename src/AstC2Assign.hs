module AstC2Assign (Assign (..)) where

import AstC2Expr (Expr)
import Data.Kind (Type)
import Var (Var)

type Assign :: Type
data Assign = Assign
  { lhs :: Var,
    rhs :: Expr
  }
  deriving stock (Show, Eq, Ord)