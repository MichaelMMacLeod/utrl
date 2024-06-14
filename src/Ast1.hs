{-# LANGUAGE DerivingStrategies #-}

module Ast1 (Ast (..), AstF (..)) where

import Data.Functor.Foldable
  ( Base,
    Corecursive,
    Recursive,
    embed,
    project,
  )
import Data.Kind (Type)
import Data.Text (Text)

type Ast :: Type
data Ast
  = Symbol Text
  | Compound [Ast]
  | Ellipses Ast
  deriving stock (Show)

type AstF :: Type -> Type
data AstF r
  = SymbolF Text
  | CompoundF [r]
  | EllipsesF r
  deriving stock (Show, Functor)

type instance Base Ast = AstF

instance Recursive Ast where
  project (Symbol s) = SymbolF s
  project (Compound xs) = CompoundF xs
  project (Ellipses x) = EllipsesF x

instance Corecursive Ast where
  embed (SymbolF s) = Symbol s
  embed (CompoundF xs) = Compound xs
  embed (EllipsesF x) = Ellipses x