module Ast0 (Ast (..), AstF (..)) where

import Control.Comonad.Cofree (Cofree (..))
import Control.Comonad.Trans.Cofree (CofreeF)
import qualified Control.Comonad.Trans.Cofree as CCTC
import Data.Functor.Foldable
  ( Base,
    Corecursive,
    Recursive (..),
    embed,
    project,
  )
import Data.List.Extra (snoc)

data Ast
  = Symbol String
  | Compound [Ast]
  deriving (Show, Eq)

data AstF r
  = SymbolF String
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