module AstC1P
  ( Ast (..),
    AstF (..),
    AssignmentLocation (..),
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

data AssignmentLocation = TopLevel | NotTopLevel
  deriving (Show, Eq)

data Ast
  = Symbol String
  | Compound [Ast]
  | Assignment (Var, Index, AssignmentLocation) Ast
  | Copy Var
  | Loop
      { var :: Var,
        src :: Var,
        start :: Int,
        end :: Int,
        body :: Ast
      }
  deriving (Show, Eq)

data IndexElement
  = ZeroPlus Int
  | LenMinus Int
  deriving (Show, Eq)

type Index = [IndexElement]

data AstF r
  = SymbolF String
  | CompoundF [r]
  | AssignmentF (Var, Index, AssignmentLocation) r
  | CopyF Var
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
  project (Assignment as x) = AssignmentF as x
  project (Copy i) = CopyF i
  project (Loop i v s e b) = LoopF i v s e b

instance Corecursive Ast where
  embed (SymbolF s) = Symbol s
  embed (CompoundF xs) = Compound xs
  embed (AssignmentF as x) = Assignment as x
  embed (CopyF i) = Copy i
  embed (LoopF i v s e b) = Loop i v s e b