module CompileTypes
  ( CompiledDefinition (..),
    Definition (..),
    VariableBindings,
  )
where

import Ast0 qualified
import AstC0 qualified
import AstC2 qualified
import AstP0 qualified
import Data.HashMap.Strict qualified as H
import Predicate (IndexedPredicate)
import ReadTypes (SrcLocked)
import ErrorTypes (Span)

data Definition = Definition
  { variables :: VariableBindings,
    pattern :: SrcLocked Ast0.Ast,
    constructor :: SrcLocked Ast0.Ast
  }

data CompiledDefinition = CompiledDefinition
  { variables :: VariableBindings,
    predicates :: [IndexedPredicate],
    pattern :: SrcLocked AstP0.Ast,
    constructor :: SrcLocked (AstC2.Ast Int)
  }

type VariableBindings = H.HashMap String (AstC0.Index, Span Int)