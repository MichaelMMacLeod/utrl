module AstP0 (Ast (..), AstF (..), indexP0ByC0) where

import AstC0 qualified
import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Comonad.Trans.Cofree qualified as C
import Data.Functor.Foldable
  ( Base,
    Corecursive,
    Recursive,
    cata,
    embed,
    project,
  )
import Data.List.Extra (snoc)
import Read (SrcLocked)
import Utils (Cata, Span (..))

data Ast
  = Symbol String
  | CompoundWithoutEllipses [Ast]
  | CompoundWithEllipses
      { before :: [Ast],
        ellipses :: Ast,
        after :: [Ast]
      }
  deriving (Show, Eq)

indexP0ByC0 :: SrcLocked Ast -> Cofree AstF (Span Int, AstC0.Index)
indexP0ByC0 ast = cata go ast []
  where
    go :: Cata (SrcLocked Ast) (AstC0.Index -> Cofree AstF (Span Int, AstC0.Index))
    go (l C.:< SymbolF s) index = (l, index) :< SymbolF s
    go (l C.:< CompoundWithoutEllipsesF xs) index =
      let xs' = zipWith (. snoc index . AstC0.ZeroPlus) xs [0 ..]
       in (l, index) :< CompoundWithoutEllipsesF xs'
    go (l C.:< CompoundWithEllipsesF b e a) index =
      let b' = zipWith (. snoc index . AstC0.ZeroPlus) b [0 ..]
          e' = e $ snoc index $ AstC0.Between (Prelude.length b) (Prelude.length a)
          a' = reverse $ zipWith (. snoc index . AstC0.LenMinus) (reverse a) [1 ..]
       in (l, index) :< CompoundWithEllipsesF b' e' a'

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