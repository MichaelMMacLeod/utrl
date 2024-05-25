{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Utils
  ( Between (..),
    iterateMaybe,
    setNth,
    Cata,
    Para,
    Histo,
    c1Tail,
    c0Head,
    popTrailingC1Index,
    popBetweenTail,
    getAtC0Index,
    tshow,
    Ana,
    replace0At,
    index0WithBase,
    index0,
    ErrorType (..),
    ErrorMessageInfo (..),
    Annotation (..),
    Span (..),
    uncofree,
  )
where

import Ast0 qualified
import AstC0 qualified
import AstC1 qualified
import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Cofree qualified as C
import Control.Comonad.Trans.Cofree (CofreeF ((:<)))
import Data.Functor.Base (ListF (..))
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.List.Extra (snoc)
import Data.Text (Text, pack)

data ErrorType
  = ParsingError
  | BadEllipsesCount
  | VarsNotCapturedUnderSameEllipsisInConstructor
  | EllipsisAppliedToSymbolInConstructor
  | InvalidRuleDefinition
  | MoreThanOneEllipsisInSingleCompoundTermOfPattern
  | VariableUsedMoreThanOnceInPattern
  | OverlappingPatterns
  deriving (Eq, Show)

data ErrorMessageInfo l = ErrorMessageInfo
  { errorType :: ErrorType,
    message :: Text,
    annotations :: [Annotation l],
    help :: Maybe Text
  }
  deriving (Eq, Show)

data Annotation l = Annotation
  { span :: Span l,
    annotation :: Text
  }
  deriving (Eq, Show)

data Span l = Span
  { location :: l,
    length :: Int
  }
  deriving (Eq, Show)

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

data Between = Between !Int !Int
  deriving (Show, Eq)

c1Tail :: AstC0.Index -> AstC1.Index
c1Tail = reverse . go . reverse
  where
    go :: AstC0.Index -> AstC1.Index
    go ((AstC0.ZeroPlus i) : xs) = AstC1.ZeroPlus i : go xs
    go ((AstC0.LenMinus i) : xs) = AstC1.LenMinus i : go xs
    go _ = []

c0Head :: AstC0.Index -> AstC0.Index
c0Head = reverse . go . reverse
  where
    go :: AstC0.Index -> AstC0.Index
    go xs@(AstC0.Between {} : _) = xs
    go (_ : xs) = go xs
    go [] = []

popTrailingC1Index :: AstC0.Index -> (AstC0.Index, AstC1.Index)
popTrailingC1Index c0 = (c0Head c0, c1Tail c0)

popBetweenTail :: AstC0.Index -> (AstC0.Index, Maybe (Int, Int))
popBetweenTail = go . reverse
  where
    go (AstC0.Between zp lm : others) = (reverse others, Just (zp, lm))
    go others = (others, Nothing)

getAtC0Index :: AstC0.Index -> Ast0.Ast -> [Ast0.Ast]
getAtC0Index [] ast = [ast]
getAtC0Index _ (Ast0.Symbol _) = []
getAtC0Index (AstC0.ZeroPlus zp : i) (Ast0.Compound xs) =
  if zp < Prelude.length xs
    then getAtC0Index i (xs !! zp)
    else []
getAtC0Index (AstC0.LenMinus lm : i) (Ast0.Compound xs) =
  let zp = Prelude.length xs - lm
   in if zp > 0
        then getAtC0Index i (xs !! zp)
        else []
getAtC0Index (AstC0.Between zp lm : i) (Ast0.Compound xs) =
  let zpEnd = Prelude.length xs - lm
   in if zp < Prelude.length xs && zpEnd > 0
        then concatMap (getAtC0Index i) (drop zp (take zpEnd xs))
        else []

tshow :: (Show a) => a -> Text
tshow = pack . show

index0Cofree :: Cofree Ast0.AstF (Span Int) -> Cofree Ast0.AstF (Span Int, [Int])
index0Cofree ast = cata go ast []
  where
    go ::
      Cata
        (Cofree Ast0.AstF (Span Int))
        ([Int] -> Cofree Ast0.AstF (Span Int, [Int]))
    go cofree index = case cofree of
      l :< Ast0.SymbolF s -> (l, index) C.:< Ast0.SymbolF s
      l :< Ast0.CompoundF xs ->
        (l, index) C.:< Ast0.CompoundF (zipWith (. snoc index) xs [0 ..])

index0WithBaseCofree :: [Int] -> Cofree Ast0.AstF (Span Int) -> Cofree Ast0.AstF (Span Int, [Int])
index0WithBaseCofree base ast = cata go ast base
  where
    go :: Cata (Cofree Ast0.AstF (Span Int)) ([Int] -> Cofree Ast0.AstF (Span Int, [Int]))
    go cofree index = case cofree of
      l :< Ast0.SymbolF s -> (l, index) C.:< Ast0.SymbolF s
      l :< Ast0.CompoundF xs ->
        (l, index) C.:< Ast0.CompoundF (zipWith (. snoc index) xs [0 ..])

-- Replaces the node at 'index' with 'replacement' in 'ast'. No
-- replacement is made if 'index' is invalid.
replace0AtCofree ::
  Cofree Ast0.AstF (Span Int, [Int]) ->
  [Int] ->
  Cofree Ast0.AstF (Span Int, [Int]) ->
  Cofree Ast0.AstF (Span Int, [Int])
replace0AtCofree ast index replacement = cata go ast
  where
    go :: Cata (Cofree Ast0.AstF (Span Int, [Int])) (Cofree Ast0.AstF (Span Int, [Int]))
    go ((l, i) :< t) =
      if i == index
        then replacement
        else (l, i) C.:< t

-- go :: CofreeF AstF [Int] Ast -> Ast
-- go (i CCTC.:< t) =
--   if i == index
--     then replacement
--     else embed t

uncofree :: (Corecursive f) => Cofree (Base f) a -> f
uncofree = cata $ \(_ :< a) -> embed a

-- uncofree :: Cofree Ast0.AstF [Int] -> Ast0.Ast
-- uncofree = cata go
--   where
--     go :: CofreeF Ast0.AstF [Int] Ast0.Ast -> Ast0.Ast
--     go (_ :< ast) = embed ast

index0 :: Ast0.Ast -> Cofree Ast0.AstF [Int]
index0 ast = cata go ast []
  where
    go :: Ast0.AstF ([Int] -> Cofree Ast0.AstF [Int]) -> [Int] -> Cofree Ast0.AstF [Int]
    go (Ast0.SymbolF s) index = index C.:< Ast0.SymbolF s
    go (Ast0.CompoundF xs) index = index C.:< Ast0.CompoundF (zipWith (. snoc index) xs [0 ..])

index0WithBase :: [Int] -> Ast0.Ast -> Cofree Ast0.AstF [Int]
index0WithBase base ast = cata go ast base
  where
    go :: Ast0.AstF ([Int] -> Cofree Ast0.AstF [Int]) -> [Int] -> Cofree Ast0.AstF [Int]
    go (Ast0.SymbolF s) index = index C.:< Ast0.SymbolF s
    go (Ast0.CompoundF xs) index = index C.:< Ast0.CompoundF (zipWith (. snoc index) xs [0 ..])

-- Replaces the node at 'index' with 'replacement' in 'ast'. No
-- replacement is made if 'index' is invalid.
replace0At :: Ast0.Ast -> [Int] -> Ast0.Ast -> Ast0.Ast
replace0At ast index replacement = cata go (index0 ast)
  where
    go :: CofreeF Ast0.AstF [Int] Ast0.Ast -> Ast0.Ast
    go (i :< t) =
      if i == index
        then replacement
        else embed t