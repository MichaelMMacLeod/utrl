module AstC2ExprBinOp (BinOp (..), Op (..)) where

import AstC2ExprVar (Var)

data BinOp = BinOp
  { op :: Op,
    lhs :: Var,
    rhs :: Var
  }

data Op = Add | Sub | ArrayAccess