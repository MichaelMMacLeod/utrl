module AstC2Expr (Expr (..)) where

import AstC2ExprVar (Var)
import Op (BinOp)

data Expr
  = Var Var
  | Constant Int
  | BinOp BinOp
  | Length Var
  | Input
  deriving (Show, Eq, Ord)