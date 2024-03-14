{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Ast0 (Ast (..), Ast0F (..)) where

import Data.Functor.Foldable
  ( Base,
    Corecursive,
    Recursive,
    embed,
    project,
  )

data Ast
  = Symbol String
  | Compound [Ast]
  deriving (Show)

data Ast0F r
  = SymbolF String
  | CompoundF [r]
  deriving (Show, Functor)

type instance Base Ast = Ast0F

instance Recursive Ast where
  project (Symbol s) = SymbolF s
  project (Compound xs) = CompoundF xs

instance Corecursive Ast where
  embed (SymbolF s) = Symbol s
  embed (CompoundF xs) = Compound xs