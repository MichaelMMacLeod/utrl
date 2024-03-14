{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module AstC1 (AstC1 (..), AstC1F (..), IndexC1, IndexElementC1 (..)) where

import Data.Functor.Foldable (Base, Corecursive, Recursive, embed, project)

data AstC1
  = SymbolC1 String
  | CompoundC1 [AstC1]
  | CopyC1 IndexC1
  | LoopC1 {indexC1 :: IndexC1, startC1 :: Integer, endC1 :: Integer, bodyC1 :: AstC1}

data AstC1F r
  = SymbolC1F String
  | CompoundC1F [r]
  | CopyC1F IndexC1
  | LoopC1F {indexC1F :: IndexC1, startC1F :: Integer, endC1F :: Integer, bodyC1F :: r}
  deriving (Functor)

type instance Base AstC1 = AstC1F

instance Recursive AstC1 where
  project (SymbolC1 s) = SymbolC1F s
  project (CompoundC1 xs) = CompoundC1F xs
  project (CopyC1 i) = CopyC1F i
  project (LoopC1 i s e b) = LoopC1F i s e b

instance Corecursive AstC1 where
  embed (SymbolC1F s) = SymbolC1 s
  embed (CompoundC1F xs) = CompoundC1 xs
  embed (CopyC1F i) = CopyC1 i
  embed (LoopC1F i s e b) = LoopC1 i s e b

data IndexElementC1
  = ZeroPlusC1 Integer
  | LenMinusC1 Integer

type IndexC1 = [IndexElementC1]