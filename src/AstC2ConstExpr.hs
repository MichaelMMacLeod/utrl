module AstC2ConstExpr (ConstExpr (..)) where

import AstC2ExprVar (Var)

data ConstExpr
  = Bool Bool
  | Var Var
  | Nat Int
  | Symbol String
  | Input
  deriving (Show, Eq, Ord)