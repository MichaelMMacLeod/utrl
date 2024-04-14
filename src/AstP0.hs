{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module AstP0 (Ast (..), AstF (..)) where

import Data.Functor.Foldable
  ( Base,
    Corecursive,
    Recursive,
    embed,
    project,
  )

data Ast
  = Symbol String
  | CompoundWithEllipses {
      before :: [Ast],
      ellipses :: Ast,
      after :: [Ast]
    }
  | CompoundWithoutEllipses [Ast]
  deriving (Show, Eq)

data AstF r
  = SymbolF String
  | CompoundWithEllipsesF {
      beforeF :: [r],
      ellipsesF :: r,
      afterF :: [r]
    }
  | CompoundWithoutEllipsesF [r]
  deriving (Show, Functor)

type instance Base Ast = AstF

instance Recursive Ast where
  project (Symbol s) = SymbolF s
  project (CompoundWithEllipses b e a) = CompoundWithEllipsesF b e a
  project (CompoundWithoutEllipses xs) = CompoundWithoutEllipsesF xs

instance Corecursive Ast where
  embed (SymbolF s) = Symbol s
  embed (CompoundWithEllipsesF b e a) = CompoundWithEllipses b e a
  embed (CompoundWithoutEllipsesF xs) = CompoundWithoutEllipses xs