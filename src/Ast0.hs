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
import GHC.Generics (Generic)
import Data.Text (Text)

data Ast
  = Symbol Text
  | Compound [Ast]
  deriving (Show, Eq, Generic, NFData)

data AstF r
  = SymbolF Text
  | CompoundF [r]
  deriving (Show, Functor)

type instance Base Ast = AstF

instance Recursive Ast where
  project (Symbol s) = SymbolF s
  project (Compound xs) = CompoundF xs

instance Corecursive Ast where
  embed :: Base Ast Ast -> Ast
  embed (SymbolF s) = Symbol s
  embed (CompoundF xs) = Compound xs