module AstC2Expr (Expr (..), BinOp_ (..), Op (..)) where

import AstC2ConstExpr (ConstExpr)

data Expr
  = ConstExpr ConstExpr
  | BinOp BinOp_
  | Length ConstExpr
  deriving (Show, Eq, Ord)

data BinOp_ = BinOp_
  { op :: Op,
    lhs :: Expr,
    rhs :: Expr
  }
  deriving (Show, Eq, Ord)

data Op = Add | Sub | ArrayAccess | LessThan
  deriving (Show, Eq, Ord)