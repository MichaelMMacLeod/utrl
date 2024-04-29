module Utils (iterateMaybe, setNth) where

import Data.Functor.Base (ListF (..))
import Data.Functor.Foldable (Corecursive (..))

iterateMaybe :: (b -> Maybe b) -> b -> [b]
iterateMaybe f b = b : ana go b
  where
    go x = case f x of
      Nothing -> Nil
      Just x' -> Cons x' x'

setNth :: Int -> a -> a -> [a] -> [a]
setNth i defaultValue x xs = left ++ [x] ++ right
  where
    left = take i (xs ++ repeat defaultValue)
    right = drop (i + 1) xs