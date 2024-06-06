module CompileTypes
  ( VariableBindings,
    Storage (..),
    DefinitionStorage (..),
    mkStorage,
    CompileRequest,
    Stage (..),
    mkRequest,
    fromSuccess,
  )
where

import Ast0 qualified
import Ast1 qualified
import AstC0 qualified
import AstC1 qualified
import AstC2 qualified
import AstP0 qualified
import Data.HashMap.Strict qualified as H
import Data.Text (Text)
import ErrorTypes (ErrorMessage, Span)
import Predicate (IndexedPredicate)
import ReadTypes (SrcLocked)
import Var (Var)

mkStorage :: [SrcLocked Ast0.Ast] -> Storage
mkStorage = Storage . map mkDefinitionStorage

mkDefinitionStorage :: SrcLocked Ast0.Ast -> DefinitionStorage
mkDefinitionStorage definition =
  DefinitionStorage
    { definition,
      constructor0 = Pending,
      constructor1 = Pending,
      constructorC0 = Pending,
      constructorC1 = Pending,
      constructorC2 = Pending,
      pattern0 = Pending,
      pattern1 = Pending,
      patternP0 = Pending,
      predicates = Pending,
      variableBindings = Pending,
      nextUnusedVar = 0
    }

newtype Storage = Storage [DefinitionStorage]

data Stage s = Pending | Success s | Fail [ErrorMessage]

data DefinitionStorage = DefinitionStorage
  { definition :: SrcLocked Ast0.Ast,
    constructor0 :: Stage (SrcLocked Ast0.Ast),
    constructor1 :: Stage (SrcLocked Ast1.Ast),
    constructorC0 :: Stage (SrcLocked AstC0.Ast),
    constructorC1 :: Stage (SrcLocked AstC1.Ast),
    constructorC2 :: Stage (SrcLocked (AstC2.Ast Int)),
    pattern0 :: Stage (SrcLocked Ast0.Ast),
    pattern1 :: Stage (SrcLocked Ast1.Ast),
    patternP0 :: Stage (SrcLocked AstP0.Ast),
    predicates :: Stage [IndexedPredicate],
    variableBindings :: Stage VariableBindings,
    nextUnusedVar :: Var
  }

mkRequest ::
  DefinitionStorage ->
  (DefinitionStorage -> Stage s) ->
  Either ([ErrorMessage], DefinitionStorage) (s, DefinitionStorage) ->
  Either ([ErrorMessage], DefinitionStorage) (s, DefinitionStorage)
mkRequest d getStage result = case getStage d of
  Fail f -> Left (f, d)
  Success s -> Right (s, d)
  Pending -> result

type CompileRequest s = DefinitionStorage -> Either ([ErrorMessage], DefinitionStorage) (s, DefinitionStorage)

fromSuccess :: Stage s -> s
fromSuccess stage = case stage of
  Success s -> s
  Pending -> error "fromScucess: Pending"
  Fail _ -> error "fromSuccess: Fail"

type VariableBindings = H.HashMap Text (AstC0.Index, Span Int)