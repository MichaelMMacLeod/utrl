module AstC2Expr (Expr (..), BinOp_ (..), Op (..)) where

import AstC2ExprVar (Var)

data Expr
  = Bool Bool
  | Var Var
  | Nat Int
  | Symbol String
  | Input
  | Length Expr
  | BinOp BinOp_
  deriving (Show, Eq, Ord)

data BinOp_ = BinOp_
  { op :: Op,
    lhs :: Expr,
    rhs :: Expr
  }
  deriving (Show, Eq, Ord)

data Op = Add | Sub | ArrayAccess | LessThan
  deriving (Show, Eq, Ord)