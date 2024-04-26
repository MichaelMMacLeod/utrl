module Stmt where

import ConstantExpr (ConstantExpr)
import Expr (Expr)
import Var (Var)

data Stmt l
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
      { term_count :: ConstantExpr
      }
  | Jump
      { label :: l
      }
  | JumpWhenLessThan
      { label :: l,
        when_var :: Var,
        le_var :: Var
      }
  deriving (Show, Functor, Eq, Ord)