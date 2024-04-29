module AstC1P
  ( Ast (..),
    AstF (..),
    Index,
    IndexElement (..),
  )
where

import AstC2ExprVar (Var)
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
      { var :: Var,
        src :: Var,
        start :: Int,
        end :: Int,
        body :: Ast
      }
  deriving (Show)

data IndexElement
  = ZeroPlus Int
  | LenMinus Int
  | Var Var
  deriving (Show)

type Index = [IndexElement]

data AstF r
  = SymbolF String
  | CompoundF [r]
  | CopyF Index
  | LoopF
      { varF :: Var,
        srcF :: Var,
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
  project (Loop i v s e b) = LoopF i v s e b

instance Corecursive Ast where
  embed (SymbolF s) = Symbol s
  embed (CompoundF xs) = Compound xs
  embed (CopyF i) = Copy i
  embed (LoopF i v s e b) = Loop i v s e b