module AstC2 (Ast, Stmt (..)) where

import AstC2Assign (Assign)
import AstC2Jump (Jump)
import AstC2Expr (Expr)

type Ast label = [Stmt label]

data Stmt label
  = Assign Assign
  | Push Expr
  | Build Expr
  | Jump (Jump label)
  deriving (Show, Functor, Eq, Ord)