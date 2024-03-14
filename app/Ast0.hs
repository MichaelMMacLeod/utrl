{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Ast0 where

import Data.Functor.Foldable (Base, Corecursive, Recursive, embed, project)

data Ast0
  = Symbol0 String
  | Compound0 [Ast0]
  deriving (Show)

data Ast0F r
  = Symbol0F String
  | Compound0F [r]
  deriving (Show, Functor)

type instance Base Ast0 = Ast0F

instance Recursive Ast0 where
  project (Symbol0 s) = Symbol0F s
  project (Compound0 xs) = Compound0F xs

instance Corecursive Ast0 where
  embed (Symbol0F s) = Symbol0 s
  embed (Compound0F xs) = Compound0 xs
