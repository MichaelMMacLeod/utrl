{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module AstC1
  ( Ast (..),
    AstF (..),
    IndexC1,
    IndexElement (..),
  )
where

import Ast0 qualified
import Data.Functor.Foldable
  ( Base,
    Corecursive,
    Recursive,
    cata,
    embed,
    project,
  )
import Data.List (intercalate)

data Ast
  = Symbol String
  | Compound [Ast]
  | Copy IndexC1
  | Loop
      { index :: IndexC1,
        start :: Integer,
        end :: Integer,
        body :: Ast
      }

data IndexElement
  = ZeroPlus Integer
  | LenMinus Integer

type IndexC1 = [IndexElement]

data AstF r
  = SymbolF String
  | CompoundF [r]
  | CopyF IndexC1
  | LoopF
      { indexF :: IndexC1,
        startF :: Integer,
        endF :: Integer,
        bodyF :: r
      }
  deriving (Functor)

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