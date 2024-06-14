module AstC2Jump (Jump (..)) where

import AstC2Expr (Expr)
import Data.Kind (Type)

type Jump :: Type -> Type
data Jump label = Jump
  { target :: label,
    condition :: Expr
  }
  deriving stock (Show, Eq, Ord, Functor)