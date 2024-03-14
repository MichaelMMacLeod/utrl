module Stmt where

import ConstantExpr (ConstantExpr)
import Expr (Expr)
import Var (Var)

data Stmt
  = Assign
      { lhs :: Var,
        rhs :: Expr
      }
  | PushSymbolToDataStack String
  | PushIndexToIndexStack ConstantExpr
  | PopFromIndexStack
      { index_count :: Int
      }
  | PushIndexedTermToDataStack
  | BuildCompoundTermFromDataStack
      { term_count :: Int
      }
  | Jump
      { label :: Int
      }
  | JumpWhenLessThan
      { label :: Int,
        when_var :: Int,
        le_var :: Int
      }
