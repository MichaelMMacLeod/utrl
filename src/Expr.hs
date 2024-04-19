module Expr where

import ConstantExpr (ConstantExpr)
import Op (BinOp)
import Var (Var)

data Expr
  = Var Var
  | Constant Int
  | BinOp
      { op :: BinOp,
        lhs :: Var,
        rhs :: ConstantExpr
      }
  | Length
  deriving (Show, Eq, Ord)