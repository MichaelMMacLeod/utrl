{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AstC0
  ( Ast (..),
    AstF (..),
    Index,
    IndexElement (..),
    isBetween,
    AstC0Between (..),
  )
where

import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Kind (Type)
import Data.Text (Text)

type Ast :: Type
data Ast
  = Symbol Text
  | Compound [Ast]
  | Ellipses Ast
  | Variable Index Text
  deriving stock (Show)

type IndexElement :: Type
data IndexElement
  = ZeroPlus Int
  | LenMinus Int
  | Between AstC0Between
  deriving stock (Eq, Show)

type AstC0Between :: Type
data AstC0Between = AstC0Between
  { zeroPlus :: Int,
    lenMinus :: Int
  }
  deriving stock (Show, Eq)

isBetween :: IndexElement -> Bool
isBetween = \case
  Between {} -> True
  _ -> False

type Index :: Type
type Index = [IndexElement]

type AstF :: Type -> Type
data AstF r
  = SymbolF Text
  | CompoundF [r]
  | VariableF Index Text
  | EllipsesF r
  deriving stock (Functor)

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