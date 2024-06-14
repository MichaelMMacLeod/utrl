module AstC2 (Ast, Stmt (..)) where

import AstC2Assign (Assign)
import AstC2Expr (Expr)
import AstC2Jump (Jump)
import Data.Kind (Type)

type Ast :: Type -> Type
type Ast label = [Stmt label]

type Stmt :: Type -> Type
data Stmt label
  = Assign Assign
  | Push Expr
  | Build Expr
  | Jump (Jump label)
  deriving stock (Show, Functor, Eq, Ord)