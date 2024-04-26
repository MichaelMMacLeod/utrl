module AstP0 (Ast (..), AstF (..), indexP0ByC0) where

import qualified AstC0
import Control.Comonad.Cofree (Cofree (..))
import Data.Functor.Foldable
  ( Base,
    Corecursive,
    Recursive,
    cata,
    embed,
    project,
  )
import Data.List.Extra (snoc)

data Ast
  = Symbol String
  | CompoundWithoutEllipses [Ast]
  | CompoundWithEllipses
      { before :: [Ast],
        ellipses :: Ast,
        after :: [Ast]
      }
  deriving (Show, Eq)

indexP0ByC0 :: Ast -> Cofree AstF AstC0.Index
indexP0ByC0 ast = cata go ast []
  where
    go ::
      AstF (AstC0.Index -> Cofree AstF AstC0.Index) ->
      (AstC0.Index -> Cofree AstF AstC0.Index)
    go (SymbolF s) index = index :< SymbolF s
    go (CompoundWithoutEllipsesF xs) index =
      let xs' = zipWith (. snoc index . AstC0.ZeroPlus) xs [0 ..]
       in index :< CompoundWithoutEllipsesF xs'
    go (CompoundWithEllipsesF b e a) index =
      let b' = zipWith (. snoc index . AstC0.ZeroPlus) b [0 ..]
          e' = e $ snoc index $ AstC0.Between (length b) (length a)
          a' = reverse $ zipWith (. snoc index . AstC0.LenMinus) (reverse a) [1 ..]
       in index :< CompoundWithEllipsesF b' e' a'

data AstF r
  = SymbolF String
  | CompoundWithoutEllipsesF [r]
  | CompoundWithEllipsesF
      { beforeF :: [r],
        ellipsesF :: r,
        afterF :: [r]
      }
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