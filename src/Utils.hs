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
  )
where

import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Trans.Cofree (CofreeF ((:<)))
import Data.Functor.Base (ListF (..))
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Text (Text, pack)

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