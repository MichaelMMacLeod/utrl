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
      { index_count :: Integer
      }
  | PushIndexedTermToDataStack
  | BuildCompoundTermFromDataStack
      { term_count :: Integer
      }
  | Jump
      { label :: Integer
      }
  | JumpWhenLessThan
      { label :: Integer,
        when_var :: Integer,
        le_var :: Integer
      }
