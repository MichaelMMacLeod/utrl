{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module AstC1
  ( Ast (..),
    AstF (..),
    Index,
    IndexElement (..),
  )
where

import Data.Functor.Foldable
  ( Base,
    Corecursive,
    Recursive,
    embed,
    project,
  )

data Ast
  = Symbol String
  | Compound [Ast]
  | Copy Index
  | Loop
      { index :: Index,
        start :: Int,
        end :: Int,
        body :: Ast
      }
  deriving (Show)

data IndexElement
  = ZeroPlus Int
  | LenMinus Int
  deriving (Show)

type Index = [IndexElement]

data AstF r
  = SymbolF String
  | CompoundF [r]
  | CopyF Index
  | LoopF
      { indexF :: Index,
        startF :: Int,
        endF :: Int,
        bodyF :: r
      }
  deriving (Functor, Show)

type instance Base Ast = AstF

instance Recursive Ast where
  project (Symbol s) = SymbolF s
  project (Compound xs) = CompoundF xs
  project (Copy i) = CopyF i
  project (Loop i s e b) = LoopF i s e b

instance Corecursive Ast where
  embed (SymbolF s) = Symbol s
  embed (CompoundF xs) = Compound xs
  embed (CopyF i) = Copy i
  embed (LoopF i s e b) = Loop i s e b