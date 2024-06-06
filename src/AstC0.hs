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
import Data.Text (Text)

data Ast
  = Symbol Text
  | Compound [Ast]
  | Ellipses Ast
  | Variable Index Text
  deriving (Show)

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
  = SymbolF Text
  | CompoundF [r]
  | VariableF Index Text
  | EllipsesF r
  deriving (Functor)

type AstF' t = AstF t -> t

type instance Base Ast = AstF

instance Recursive Ast where
  project (Symbol s) = SymbolF s
  project (Compound xs) = CompoundF xs
  project (Variable i n) = VariableF i n
  project (Ellipses x) = EllipsesF x

instance Corecursive Ast where
  embed (SymbolF s) = Symbol s
  embed (CompoundF xs) = Compound xs
  embed (VariableF i n) = Variable i n
  embed (EllipsesF x) = Ellipses x