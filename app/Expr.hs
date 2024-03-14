module Expr where

import ConstantExpr (ConstantExpr)
import Op (BinOp)
import Var (Var)

data Expr
  = Var Var
  | Constant Integer
  | BinOp
      { op :: BinOp,
        lhs :: Var,
        rhs :: ConstantExpr
      }
  | Length Var