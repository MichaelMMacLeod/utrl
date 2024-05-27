{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Utils
  ( iterateMaybe,
    setNth,
    Cata,
    Para,
    Histo,
    tshow,
    Ana,
    uncofree,
    isDollarSignVar,
    popBetweenTail,
    popTrailingC1Index,
    c0Head,
    c1Tail,
    Between (..),
    getPatternSpanAtC0Index,
    pushBetweenTail,
  )
where

import Ast1 qualified
import AstC0 qualified
import AstC1 qualified
import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Trans.Cofree (CofreeF ((:<)))
import Data.Functor.Base (ListF (..))
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.List.Extra (snoc, (!?))
import Data.Text (Text, pack)
import ErrorTypes (Span)
import ReadTypes (SrcLocked)

type Cata t a = Base t a -> a

type Para t a = Base t (t, a) -> a

type Histo t a = Base t (Cofree (Base t) a) -> a

type Ana t a = a -> Base t a

iterateMaybe :: (b -> Maybe b) -> b -> [b]
iterateMaybe f b = b : ana go b
  where
    go x = case f x of
      Nothing -> Nil
      Just x' -> Cons x' x'

setNth :: Int -> (Int -> a) -> a -> [a] -> [a]
setNth i defaultValue x xs = left ++ [x] ++ right
  where
    left = take i (xs ++ map defaultValue [0 ..])
    right = drop (i + 1) xs

tshow :: (Show a) => a -> Text
tshow = pack . show

-- | Removes 'Cofree' labels from a recursive type.
-- 'Cofree' is used in this project to label AST nodes with their indices (see 'index0').
-- This function can remove those index labels, returning the original AST.
uncofree :: (Corecursive f) => Cofree (Base f) a -> f
uncofree = cata $ \(_ :< a) -> embed a

isDollarSignVar :: String -> Bool
isDollarSignVar ('$' : _) = True
isDollarSignVar _ = False

-- | Returns the C1-portion of the end of a C0-index, that is,
-- drops all indices up to and including the last 'Between' variant
-- of the input C0-index. Because all 'Between' variants are dropped,
-- the resulting index is a C1-index.
c1Tail :: AstC0.Index -> AstC1.Index
c1Tail = reverse . go . reverse
  where
    go :: AstC0.Index -> AstC1.Index
    go ((AstC0.ZeroPlus i) : xs) = AstC1.ZeroPlus i : go xs
    go ((AstC0.LenMinus i) : xs) = AstC1.LenMinus i : go xs
    go _ = []

-- | Returns the C0-portion of the start of a C0-index, that is,
-- takes all indices up to and including the last 'Between' variant
-- of the input C0-index.
c0Head :: AstC0.Index -> AstC0.Index
c0Head = reverse . go . reverse
  where
    go :: AstC0.Index -> AstC0.Index
    go xs@(AstC0.Between {} : _) = xs
    go (_ : xs) = go xs
    go [] = []

-- | Subdivides a C0-index into its initial C0 portion and its
-- trailing C1 portion.
popTrailingC1Index :: AstC0.Index -> (AstC0.Index, AstC1.Index)
popTrailingC1Index c0 = (c0Head c0, c1Tail c0)

-- | Returns the 'Between' 'IndexElement' at the end of the input
-- C0-index, or Nothing if the input index ends with a different
-- 'IndexElement. The first elemnet of the returned pair is the
-- rest of the input.
popBetweenTail :: AstC0.Index -> (AstC0.Index, Maybe Between)
popBetweenTail = go . reverse
  where
    go (AstC0.Between zp lm : others) = (reverse others, Just $ Between zp lm)
    go others = (others, Nothing)

data Between = Between
  { zeroPlus :: Int,
    lenMinus :: Int
  }
  deriving (Show, Eq)

pushBetweenTail :: (AstC0.Index, Between) -> AstC0.Index
pushBetweenTail (index, between) = snoc index (AstC0.Between between.zeroPlus between.lenMinus)

getPatternSpanAtC0Index :: SrcLocked Ast1.Ast -> AstC0.Index -> Maybe (Span Int)
getPatternSpanAtC0Index = cata go
  where
    go :: Cata (SrcLocked Ast1.Ast) (AstC0.Index -> Maybe (Span Int))
    go (span :< ast) index =
      case index of
        [] -> Just span
        indexHead : indexTail ->
          case ast of
            Ast1.SymbolF _ -> Nothing
            Ast1.CompoundF xs ->
              case indexHead of
                AstC0.ZeroPlus n -> do
                  x <- xs !? n
                  x indexTail
                AstC0.LenMinus n -> do
                  x <- xs !? (length xs - n)
                  x indexTail
                AstC0.Between n _ -> do
                  x <- xs !? n
                  x index
            Ast1.EllipsesF x ->
              case indexHead of
                AstC0.Between _ _ -> x indexTail
                _ -> Nothing