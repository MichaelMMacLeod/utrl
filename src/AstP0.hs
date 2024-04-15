{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module AstP0 (Ast (..), AstF (..), indexP0ByC0) where

import AstC0 qualified
import Control.Comonad.Cofree (Cofree (..))
import Control.Monad.Reader (MonadReader (ask), Reader, runReader, withReader)
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

type RR = Reader AstC0.Index (Cofree AstF AstC0.Index)

indexP0ByC0 :: Ast -> Cofree AstF AstC0.Index
indexP0ByC0 = flip runReader [] . cata go
  where
    go :: AstF RR -> RR
    go = \case
      SymbolF s -> do
        index <- ask
        pure $ index :< SymbolF s
      CompoundWithoutEllipsesF xs -> do
        index <- ask
        let xs' = map (`runReader` []) $ zipWith (setZeroPlusReader index) [0 ..] xs
        pure $ index :< CompoundWithoutEllipsesF xs'
      CompoundWithEllipsesF b e a -> do
        index <- ask
        let b' = map (`runReader` []) $ zipWith (setZeroPlusReader index) [0 ..] b
            e' = runReader (setBetweenReader index (length b) (length a) e) []
            a' = map (`runReader` []) $ reverse $ zipWith (setLenMinusReader index) [1 ..] $ reverse a
        pure $ index :< CompoundWithEllipsesF b' e' a'

    setZeroPlusReader,
      setLenMinusReader ::
        AstC0.Index ->
        Int ->
        Reader AstC0.Index a ->
        Reader AstC0.Index a
    setZeroPlusReader index i = withReader $ const $ snoc index $ AstC0.ZeroPlus i
    setLenMinusReader index i = withReader $ const $ snoc index $ AstC0.LenMinus i

    setBetweenReader ::
      AstC0.Index ->
      Int ->
      Int ->
      Reader AstC0.Index a ->
      Reader AstC0.Index a
    setBetweenReader index b a = withReader $ const $ snoc index $ AstC0.Between b a

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