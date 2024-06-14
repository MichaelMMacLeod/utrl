module AstC2Expr
  ( Expr (..),
    Op (..),
    ExprF (..),
  )
where

import AstC2ExprVar (Var)
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Kind (Type)
import Data.Text (Text)

type Expr :: Type
data Expr
  = Bool Bool
  | Var Var
  | Nat Int
  | Symbol Text
  | Input
  | Length Expr
  | BinOp Op Expr Expr
  deriving stock (Show, Eq, Ord)

type Op :: Type
data Op = Add | Sub | ArrayAccess | LessThan
  deriving stock (Show, Eq, Ord)

type ExprF :: Type -> Type
data ExprF r
  = BoolF Bool
  | VarF Var
  | NatF Int
  | SymbolF Text
  | InputF
  | LengthF r
  | BinOpF Op r r
  deriving stock (Show, Eq, Ord, Functor)

type instance Base Expr = ExprF

instance Recursive Expr where
  project = \case
    Bool b -> BoolF b
    Var v -> VarF v
    Nat n -> NatF n
    Symbol s -> SymbolF s
    Input -> InputF
    Length l -> LengthF l
    BinOp op lhs rhs -> BinOpF op lhs rhs

instance Corecursive Expr where
  embed = \case
    BoolF b -> Bool b
    VarF v -> Var v
    NatF n -> Nat n
    SymbolF s -> Symbol s
    InputF -> Input
    LengthF l -> Length l
    BinOpF op lhs rhs -> BinOp op lhs rhs