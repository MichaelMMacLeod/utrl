module CompileTypes
  ( VariableBindings,
    Storage (..),
    DefinitionStorage (..),
    mkStorage,
    CompileRequest,
  )
where

import Ast0 qualified
import Ast1 qualified
import AstC0 qualified
import AstC1 qualified
import AstC2 qualified
import AstP0 qualified
import Data.HashMap.Strict qualified as H
import Error (CompileResult)
import ErrorTypes (Span)
import Predicate (IndexedPredicate)
import ReadTypes (SrcLocked)
import Var (Var)

mkStorage :: [SrcLocked Ast0.Ast] -> Storage
mkStorage = Storage . map mkDefinitionStorage

mkDefinitionStorage :: SrcLocked Ast0.Ast -> DefinitionStorage
mkDefinitionStorage definition =
  DefinitionStorage
    { definition,
      constructor0 = Nothing,
      constructor1 = Nothing,
      constructorC0 = Nothing,
      constructorC1 = Nothing,
      constructorC2 = Nothing,
      pattern0 = Nothing,
      pattern1 = Nothing,
      patternP0 = Nothing,
      predicates = Nothing,
      variableBindings = Nothing,
      nextUnusedVar = 0
    }

newtype Storage = Storage [DefinitionStorage]

data DefinitionStorage = DefinitionStorage
  { definition :: SrcLocked Ast0.Ast,
    constructor0 :: Maybe (SrcLocked Ast0.Ast),
    constructor1 :: Maybe (SrcLocked Ast1.Ast),
    constructorC0 :: Maybe (SrcLocked AstC0.Ast),
    constructorC1 :: Maybe (SrcLocked AstC1.Ast),
    constructorC2 :: Maybe (SrcLocked (AstC2.Ast Int)),
    pattern0 :: Maybe (SrcLocked Ast0.Ast),
    pattern1 :: Maybe (SrcLocked Ast1.Ast),
    patternP0 :: Maybe (SrcLocked AstP0.Ast),
    predicates :: Maybe [IndexedPredicate],
    variableBindings :: Maybe VariableBindings,
    nextUnusedVar :: Var
  }

type CompileRequest a = DefinitionStorage -> CompileResult (a, DefinitionStorage)

type VariableBindings = H.HashMap String (AstC0.Index, Span Int)