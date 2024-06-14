module AstP0
  ( Ast (..),
    AstF (..),
    indexP0ByC0,
    AstP0CompoundWtihEllipses (..),
    AstP0CompoundWtihEllipsesF (..),
  )
where

import AstC0 (AstC0Between (AstC0Between))
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
import Data.Kind (Type)
import Data.List.Extra (snoc)
import Data.Text (Text)
import ErrorTypes (Span (..))
import ReadTypes (SrcLocked)
import Utils (Cata)

type Ast :: Type
data Ast
  = Symbol Text
  | CompoundWithoutEllipses [Ast]
  | CompoundWithEllipses AstP0CompoundWtihEllipses
  deriving stock (Show, Eq)

type AstP0CompoundWtihEllipses :: Type
data AstP0CompoundWtihEllipses = AstP0CompoundWtihEllipses
  { before :: [Ast],
    ellipses :: Ast,
    after :: [Ast]
  }
  deriving stock (Show, Eq)

indexP0ByC0 :: SrcLocked Ast -> Cofree AstF (Span Int, AstC0.Index)
indexP0ByC0 ast = cata go ast []
  where
    go :: Cata (SrcLocked Ast) (AstC0.Index -> Cofree AstF (Span Int, AstC0.Index))
    go (l C.:< SymbolF s) index = (l, index) :< SymbolF s
    go (l C.:< CompoundWithoutEllipsesF xs) index =
      let xs' = zipWith (. snoc index . AstC0.ZeroPlus) xs [0 ..]
       in (l, index) :< CompoundWithoutEllipsesF xs'
    go (l C.:< CompoundWithEllipsesF (AstP0CompoundWtihEllipsesF b e a)) index =
      let b' = zipWith (. snoc index . AstC0.ZeroPlus) b [0 ..]
          e' = e $ snoc index $ AstC0.Between $ AstC0Between (Prelude.length b) (Prelude.length a)
          a' = reverse $ zipWith (. snoc index . AstC0.LenMinus) (reverse a) [1 ..]
       in (l, index) :< CompoundWithEllipsesF (AstP0CompoundWtihEllipsesF b' e' a')

type AstF :: Type -> Type
data AstF r
  = SymbolF Text
  | CompoundWithoutEllipsesF [r]
  | CompoundWithEllipsesF (AstP0CompoundWtihEllipsesF r)
  deriving stock (Show, Functor)

type AstP0CompoundWtihEllipsesF :: Type -> Type
data AstP0CompoundWtihEllipsesF r = AstP0CompoundWtihEllipsesF
  { beforeF :: [r],
    ellipsesF :: r,
    afterF :: [r]
  }
  deriving stock (Show, Functor)

type instance Base Ast = AstF

instance Recursive Ast where
  project (Symbol s) = SymbolF s
  project (CompoundWithEllipses (AstP0CompoundWtihEllipses b e a)) =
    CompoundWithEllipsesF (AstP0CompoundWtihEllipsesF b e a)
  project (CompoundWithoutEllipses xs) = CompoundWithoutEllipsesF xs

instance Corecursive Ast where
  embed (SymbolF s) = Symbol s
  embed (CompoundWithEllipsesF (AstP0CompoundWtihEllipsesF b e a)) =
    CompoundWithEllipses (AstP0CompoundWtihEllipses b e a)
  embed (CompoundWithoutEllipsesF xs) = CompoundWithoutEllipses xs