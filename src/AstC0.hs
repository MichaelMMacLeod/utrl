{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AstC0
  ( Ast (..),
    AstF (..),
    AstF',
    Index,
    IndexElement (..),
    isBetween,
  )
where

import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))

data Ast
  = Symbol String
  | Compound [Ast]
  | Ellipses Ast
  | Variable (Index, String)

data IndexElement
  = ZeroPlus Int
  | LenMinus Int
  | Between
      { zeroPlus :: Int,
        lenMinus :: Int
      }
  deriving (Eq, Show, Ord)

isBetween :: IndexElement -> Bool
isBetween = \case
  Between {} -> True
  _ -> False

type Index = [IndexElement]

data AstF r
  = SymbolF String
  | CompoundF [r]
  | VariableF (Index, String)
  | EllipsesF r
  deriving (Functor)

type AstF' t = AstF t -> t

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