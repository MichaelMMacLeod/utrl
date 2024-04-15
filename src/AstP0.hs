{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module AstP0 (Ast (..), AstF (..), enumerateP0, enumerateP0Recursively) where

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

data Ast
  = Symbol String
  | CompoundWithoutEllipses [Ast]
  | CompoundWithEllipses
      { before :: [Ast],
        ellipses :: Ast,
        after :: [Ast]
      }
  deriving (Show, Eq)

-- Finds the first element in a list that satisfies a predicate,
-- returning the elements before it, itself, and the elements that
-- follow it. Nothing is returned if no element satisfies the predicate.
splitBeforeAndAfter :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
splitBeforeAndAfter p = go []
  where
    go acc (x : xs)
      | p x = Just (reverse acc, x, xs)
      | otherwise = go (x : acc) xs
    go _ [] = Nothing

subindexZP :: [AstC0.IndexElement] -> Int -> [AstC0.IndexElement]
subindexZP i e = i ++ [AstC0.ZeroPlus e]

subindexLM :: [AstC0.IndexElement] -> Int -> [AstC0.IndexElement]
subindexLM i e = i ++ [AstC0.LenMinus e]

enumerateP0 :: AstC0.Index -> Ast -> AstF (AstC0.Index, Ast)
enumerateP0 _ (Symbol s) = SymbolF s
enumerateP0 index (CompoundWithoutEllipses xs) =
  CompoundWithoutEllipsesF (zip (map (subindexZP index) [0 ..]) xs)
enumerateP0 index (CompoundWithEllipses b e a) =
  let b' = zip (map (subindexZP index) [0 ..]) b
      e' = (index ++ [AstC0.Between (length b) (length a)], e)
      a' = reverse $ zip (map (subindexLM index) [1 ..]) $ reverse b
   in CompoundWithEllipsesF b' e' a'

type RR = Reader AstC0.Index (Cofree AstF AstC0.Index)

enumerateP0Recursively :: Ast -> Cofree AstF AstC0.Index
enumerateP0Recursively = flip runReader [] . cata go
  where
    go :: AstF RR -> RR
    go = \case
      SymbolF s -> do
        index <- ask
        pure $ index :< SymbolF s
      CompoundWithoutEllipsesF xs -> do
        index <- ask
        let xs' = map (`runReader` []) $ zipWith (\i r -> withReader (const (index ++ [AstC0.ZeroPlus i])) r) [0 ..] xs
        pure $ index :< CompoundWithoutEllipsesF xs'
      CompoundWithEllipsesF b e a -> do
        index <- ask
        let b' = map (`runReader` []) $ zipWith (\i r -> withReader (const (index ++ [AstC0.ZeroPlus i])) r) [0 ..] b
            e' = runReader (withReader (const (index ++ [AstC0.Between (length b) (length a)])) e) []
            a' = map (`runReader` []) $ reverse $ zipWith (\i r -> withReader (const (index ++ [AstC0.LenMinus i])) r) [1 ..] $ reverse a
        pure $ index :< CompoundWithEllipsesF b' e' a'

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