module AstC2Expr (Expr (..)) where

import AstC2ConstExpr (ConstExpr)
import AstC2ExprBinOp (BinOp)

data Expr
  = ConstExpr ConstExpr
  | BinOp BinOp
  | Length ConstExpr
  deriving (Show, Eq, Ord)