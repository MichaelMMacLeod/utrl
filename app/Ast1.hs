{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Ast1 (Ast (..), AstF (..)) where

import Ast0 qualified
import Data.Functor.Foldable
  ( Base,
    Corecursive,
    Recursive,
    cata,
    embed,
    project,
  )

data Ast
  = Symbol String
  | Compound [Ast]
  | Ellipses Ast
  deriving (Show)

data AstF r
  = SymbolF String
  | CompoundF [r]
  | EllipsesF r
  deriving (Show, Functor)

type instance Base Ast = AstF

instance Recursive Ast where
  project (Symbol s) = SymbolF s
  project (Compound xs) = CompoundF xs
  project (Ellipses x) = EllipsesF x

instance Corecursive Ast where
  embed (SymbolF s) = Symbol s
  embed (CompoundF xs) = Compound xs
  embed (EllipsesF x) = Ellipses x