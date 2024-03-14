{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module AstC0 (Ast (..), AstF (..), IndexC0, IndexElement (..)) where

import Data.Functor.Foldable (Base, Corecursive, Recursive, embed, project)

data Ast
  = Symbol String
  | Compound [Ast]
  | Ellipses Ast
  | Variable IndexC0

data AstF r
  = SymbolF String
  | CompoundF [r]
  | VariableF IndexC0
  | EllipsesF r
  deriving (Functor)

type instance Base Ast = AstF

instance Recursive Ast where
  project (Symbol s) = SymbolF s
  project (Compound xs) = CompoundF xs
  project (Variable i) = VariableF i
  project (Ellipses x) = EllipsesF x

instance Corecursive Ast where
  embed (SymbolF s) = Symbol s
  embed (CompoundF xs) = Compound xs
  embed (VariableF i) = Variable i
  embed (EllipsesF x) = Ellipses x

data IndexElement
  = ZeroPlusC0 Integer
  | LenMinusC0 Integer
  | BetweenC0 {zeroPlusC0 :: Integer, lenMinusC0 :: Integer}
  deriving (Eq)

type IndexC0 = [IndexElement]