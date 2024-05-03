{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Utils
  ( Between (..),
    iterateMaybe,
    setNth,
    classifyC0,
  )
where

import qualified AstC0
import qualified AstC1
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

classifyC0 ::
  AstC0.Index ->
  Either
    AstC1.Index
    ( AstC0.Index,
      Between,
      AstC1.Index
    )
classifyC0 i =
  let (c0, c1) = AstC0.popTrailingC1Index i
   in if null c0
        then Left c1
        else case AstC0.popBetweenTail c0 of
          (c0, Just (zeroPlus, lenMinus)) ->
            Right (c0, Between zeroPlus lenMinus, c1)
          (_, Nothing) -> error "unreachable"

data Between = Between !Int !Int
  deriving (Show, Eq)

-- data C0Classification
--   = ClassC1 AstC1.Index
--   | ClassC0C1 AstC0.Index Between AstC1.Index

-- data Index2Element = ZeroPlus Int | LenMinus Int

-- newtype Index2 = Index2 [Index2Element]

-- data Index3 = Index3 Index2 Between

-- data Index4
--   = Index4Really2 [Index2Element]
--   | Index4Really3Or2 [Index3]