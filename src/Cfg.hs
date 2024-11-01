{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Cfg (mkCfg, Cfg (..), compile) where

import Analyze (analyzeOverlappingPatterns)
import Ast0 qualified
import AstC2 qualified
import Compile
  ( errorsToEither,
    requestConstructorC2,
    requestPatternP0,
    requestPredicates,
    requestVariableBindings,
  )
import CompileTypes (CompileRequest, DefinitionStorage, Storage (..), mkStorage)
import Data.Either.Extra (mapLeft)
import Data.Functor.Base (ListF (..))
import Data.Functor.Foldable (Recursive (..))
import Data.Graph.Inductive (Graph (mkGraph), Node)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Kind (Type)
import Error (CompileResult (CompileResult))
import ErrorTypes (ErrorMessage)
import Predicate (IndexedPredicate)
import ReadTypes (SrcLocked)
import Utils (Cata, stripSourceLocationInfo)

compile :: [SrcLocked Ast0.Ast] -> CompileResult Cfg
compile defAsts =
  case mkCfg $ mkStorage defAsts of
    Left errors -> CompileResult (Storage []) (Left errors)
    Right (storage, cfg) -> CompileResult storage (Right cfg)

-- It's tempting to use 'mapM' to evaluate each compileRequest against every definition,
-- but due to the way that 'mapM' works with 'Either', this would short-circuit at the
-- first error, resulting in only errors from the first erroneous definition beind displayed.
-- We want to see all the errors, so we can't just use 'mapM' here.
gatherAllErrorsOrResults ::
  CompileRequest s ->
  [DefinitionStorage] ->
  Either
    [ErrorMessage]
    [(s, DefinitionStorage)]
gatherAllErrorsOrResults compileRequest definitionStorages =
  cata go $ map (mapLeft fst . compileRequest) definitionStorages
  where
    go ::
      Cata
        [ Either
            [ErrorMessage]
            (s, DefinitionStorage)
        ]
        (Either [ErrorMessage] [(s, DefinitionStorage)])
    go = \case
      Nil -> Right []
      Cons lr result ->
        case lr of
          Left errors -> case result of
            Left resultErrors ->
              Left $ errors <> resultErrors
            Right _ ->
              Left errors
          Right successDsPair -> case result of
            Left errors ->
              Left errors
            Right successDsPairs ->
              Right $ successDsPair : successDsPairs

mkCfg :: Storage -> Either [ErrorMessage] (Storage, Cfg)
mkCfg (Storage definitionStorages) = do
  (constructorC2s, definitionStorages) <-
    unzip <$> gatherAllErrorsOrResults requestConstructorC2 definitionStorages
  (predicates, definitionStorages) <-
    unzip <$> gatherAllErrorsOrResults requestPredicates definitionStorages
  (p0s, definitionStorages) <-
    unzip <$> gatherAllErrorsOrResults requestPatternP0 definitionStorages
  (variableBindings, definitionStorages) <-
    unzip <$> gatherAllErrorsOrResults requestVariableBindings definitionStorages
  errorsToEither . analyzeOverlappingPatterns $ zip variableBindings p0s
  let start :: Int
      start = 0

      lnodes :: [(Int, AstC2.Ast Int)]
      lnodes = (start, []) : zip [(start + 1) ..] (map stripSourceLocationInfo constructorC2s)

      ledges :: [(Int, Int, [IndexedPredicate])]
      ledges = zipWith (\i p -> (start, i, p)) [(start + 1) ..] predicates

      gr :: Gr (AstC2.Ast Int) [IndexedPredicate]
      gr = mkGraph lnodes ledges
  Right (Storage definitionStorages, Cfg gr start)

type Cfg :: Type
data Cfg = Cfg
  { graph :: !(Gr (AstC2.Ast Int) [IndexedPredicate]),
    start :: !Node
  }
  deriving stock (Show)

-- dumpCfgStmts :: Cfg -> String
-- dumpCfgStmts = intercalate "\n\n" . map (Display.displayC2 . snd) . labNodes . graph