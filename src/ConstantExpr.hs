module ConstantExpr (ConstantExpr (..)) where

import Var (Var)

data ConstantExpr
  = Var Var
  | Constant Int
  deriving (Show)