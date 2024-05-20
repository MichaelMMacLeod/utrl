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
  )
where

import qualified Ast0
import qualified AstC0
import qualified AstC1
import Control.Comonad.Cofree (Cofree)
import Data.Functor.Base (ListF (..))
import Data.Functor.Foldable (Base, Corecursive (..))
import Data.Text (Text, pack)

type Cata t a = Base t a -> a

type Para t a = Base t (t, a) -> a

type Histo t a = Base t (Cofree (Base t) a) -> a

iterateMaybe :: (b -> Maybe b) -> b -> [b]
iterateMaybe f b = b : ana go b
  where
    go x = case f x of
      Nothing -> Nil
      Just x' -> Cons x' x'

setNth :: Int -> (Int -> a) -> a -> [a] -> [a]
setNth i defaultValue x xs = left ++ [x] ++ right
  where
    left = take i (xs ++ map defaultValue [0..])
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
  if zp < length xs
    then getAtC0Index i (xs !! zp)
    else []
getAtC0Index (AstC0.LenMinus lm : i) (Ast0.Compound xs) =
  let zp = length xs - lm
   in if zp > 0
        then getAtC0Index i (xs !! zp)
        else []
getAtC0Index (AstC0.Between zp lm : i) (Ast0.Compound xs) =
  let zpEnd = length xs - lm
   in if zp < length xs && zpEnd > 0
        then concatMap (getAtC0Index i) (drop zp (take zpEnd xs))
        else []

tshow :: Show a => a -> Text
tshow = pack . show