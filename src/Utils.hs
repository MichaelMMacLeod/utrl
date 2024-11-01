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
    getPatternSpanAtC0Index,
    pushBetweenTail,
    compareSpan,
    flipOrder,
    intToText,
    stripSourceLocationInfo,
    spanOf,
    shortSpan,
    shortSpanOf,
  )
where

import Ast1 qualified
import AstC0 (AstC0Between (..))
import AstC0 qualified
import AstC1 qualified
import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Cofree qualified as CC
import Control.Comonad.Trans.Cofree (CofreeF ((:<)))
import Data.Functor.Base (ListF (..))
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Kind (Type)
import Data.List.Extra (snoc, (!?))
import Data.Text (Text, isPrefixOf, pack)
import Data.Text.Lazy qualified as T (toStrict)
import Data.Text.Lazy.Builder qualified as B
import Data.Text.Lazy.Builder.Int qualified as B
import ErrorTypes (Span, location)
import ErrorTypes qualified as ET
import GHC.Base (compareInt)
import ReadTypes (SrcLocked)

type Cata :: Type -> Type -> Type
type Cata t a = Base t a -> a

type Para :: Type -> Type -> Type
type Para t a = Base t (t, a) -> a

type Histo :: Type -> Type -> Type
type Histo t a = Base t (Cofree (Base t) a) -> a

type Ana :: Type -> Type -> Type
type Ana a t = a -> Base t a

iterateMaybe :: forall b. (b -> Maybe b) -> b -> [b]
iterateMaybe f b = b : ana go b
  where
    go :: Ana b [b]
    go x = case f x of
      Nothing -> Nil
      Just x' -> Cons x' x'

setNth :: Int -> (Int -> a) -> a -> [a] -> [a]
setNth i defaultValue x xs = left ++ [x] ++ right
  where
    left = take i (xs ++ map defaultValue [0 ..])
    right = drop (i + 1) xs

-- Converts integers to Text without converting through String.
-- Copied from here: https://github.com/haskell/text/issues/218
intToText :: (Integral a) => a -> Text
intToText = T.toStrict . B.toLazyText . B.decimal

tshow :: (Show a) => a -> Text
tshow = pack . show

-- | Removes 'Cofree' labels from a recursive type.
uncofree :: (Corecursive f) => Cofree (Base f) a -> f
uncofree = cata $ \(_ :< a) -> embed a

-- | Removes source location information from each node of an AST.
stripSourceLocationInfo :: (Corecursive f) => Cofree (Base f) (Span l) -> f
stripSourceLocationInfo = uncofree

spanOf :: Cofree f (Span l) -> Span l
spanOf (s CC.:< _) = s

shortSpan :: Span l -> Span l
shortSpan s = s {ET.length = 1}

shortSpanOf :: Cofree f (Span l) -> Span l
shortSpanOf = shortSpan . spanOf

isDollarSignVar :: Text -> Bool
isDollarSignVar = isPrefixOf "$"

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
popBetweenTail :: AstC0.Index -> (AstC0.Index, Maybe AstC0Between)
popBetweenTail = go . reverse
  where
    go (AstC0.Between (AstC0Between zp lm) : others) = (reverse others, Just $ AstC0Between zp lm)
    go others = (others, Nothing)

pushBetweenTail :: (AstC0.Index, AstC0Between) -> AstC0.Index
pushBetweenTail (index, between) = snoc index (AstC0.Between (AstC0Between between.zeroPlus between.lenMinus))

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
                AstC0.Between (AstC0Between n _) -> do
                  x <- xs !? n
                  x indexTail
            Ast1.EllipsesF x -> x index

compareSpan :: Span Int -> Span Int -> Ordering
compareSpan s1 s2 = compareInt s1.location s2.location

flipOrder :: Ordering -> Ordering
flipOrder = \case
  GT -> LT
  LT -> GT
  EQ -> EQ