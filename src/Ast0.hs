{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Ast0 (Ast (..), AstF (..)) where

import Control.DeepSeq (NFData)
import Data.Functor.Foldable
  ( Base,
    Corecursive,
    Recursive (..),
    embed,
    project,
  )
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)

type Ast :: Type
data Ast
  = Symbol Text
  | Compound [Ast]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

type AstF :: Type -> Type
data AstF r
  = SymbolF Text
  | CompoundF [r]
  deriving stock (Show, Functor)

type instance Base Ast = AstF

instance Recursive Ast where
  project (Symbol s) = SymbolF s
  project (Compound xs) = CompoundF xs

instance Corecursive Ast where
  embed :: Base Ast Ast -> Ast
  embed (SymbolF s) = Symbol s
  embed (CompoundF xs) = Compound xs