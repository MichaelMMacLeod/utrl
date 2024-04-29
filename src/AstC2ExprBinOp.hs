module AstC2ExprBinOp (BinOp (..), Op (..)) where

import AstC2ConstExpr (ConstExpr)
import AstC2ExprVar (Var)

data BinOp = BinOp
  { op :: Op,
    lhs :: Var,
    rhs :: ConstExpr
  }
  deriving (Show, Eq, Ord)

data Op = Add | Sub | ArrayAccess | LessThan
  deriving (Show, Eq, Ord)