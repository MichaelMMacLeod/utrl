module Ast0 (Ast (..), AstF (..), index0, replace0At) where

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

index0 :: Ast -> Cofree AstF [Int]
index0 ast = cata go ast []
  where
    go :: AstF ([Int] -> Cofree AstF [Int]) -> [Int] -> Cofree AstF [Int]
    go (SymbolF s) index = index :< SymbolF s
    go (CompoundF xs) index = index :< CompoundF (zipWith (. snoc index) xs [0 ..])

-- Replaces the node at 'index' with 'replacement' in 'ast'. No
-- replacement is made if 'index' is invalid.
replace0At :: Ast -> [Int] -> Ast -> Ast
replace0At ast index replacement = cata go (index0 ast)
  where
    go :: CofreeF AstF [Int] Ast -> Ast
    go (i CCTC.:< t) =
      if i == index
        then replacement
        else embed t

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