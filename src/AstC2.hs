module AstC2 (Ast, Stmt (..)) where

import AstC2Assign (Assign)
import AstC2Jump (Jump)
import Var (Var)

type Ast l = [Stmt l]

data Stmt l
  = Assign Assign
  | Push Var
  | Build Int
  | Jump (Jump l)
  | UnconditionalJump l
  deriving (Show, Functor, Eq, Ord)