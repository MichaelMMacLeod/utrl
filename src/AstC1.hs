module AstC1
  ( Ast (..),
    AstF (..),
    AssignmentLocation (..),
    Index,
    IndexElement (..),
    AstC1Loop (..),
    AstC1LoopF (..),
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
import Data.Kind (Type)
import Data.Text (Text)

type AssignmentLocation :: Type
data AssignmentLocation = TopLevel | NotTopLevel
  deriving stock (Show, Eq)

type Ast :: Type
data Ast
  = Symbol Text
  | Compound [Ast]
  | Assignment (Var, Index, AssignmentLocation) Ast
  | Copy Var
  | Loop AstC1Loop
  deriving stock (Show, Eq)

type AstC1Loop :: Type
data AstC1Loop = AstC1Loop
  { var :: Var,
    src :: Var,
    start :: Int,
    end :: Int,
    body :: Ast
  }
  deriving stock (Show, Eq)

type IndexElement :: Type
data IndexElement
  = ZeroPlus Int
  | LenMinus Int
  deriving stock (Show, Eq)

type Index :: Type
type Index = [IndexElement]

type AstF :: Type -> Type
data AstF r
  = SymbolF Text
  | CompoundF [r]
  | AssignmentF (Var, Index, AssignmentLocation) r
  | CopyF Var
  | LoopF (AstC1LoopF r)
  deriving stock (Functor, Show)

type AstC1LoopF :: Type -> Type
data AstC1LoopF r = AstC1LoopF
  { varF :: Var,
    srcF :: Var,
    startF :: Int,
    endF :: Int,
    bodyF :: r
  }
  deriving stock (Functor, Show)

type instance Base Ast = AstF

instance Recursive Ast where
  project (Symbol s) = SymbolF s
  project (Compound xs) = CompoundF xs
  project (Assignment as x) = AssignmentF as x
  project (Copy i) = CopyF i
  project (Loop (AstC1Loop i v s e b)) = LoopF (AstC1LoopF i v s e b)

instance Corecursive Ast where
  embed (SymbolF s) = Symbol s
  embed (CompoundF xs) = Compound xs
  embed (AssignmentF as x) = Assignment as x
  embed (CopyF i) = Copy i
  embed (LoopF (AstC1LoopF i v s e b)) = Loop (AstC1Loop i v s e b)