{-
The functions in this file perform syntactic and semantic analysis at various stages
of compilation, returning informative error messages should errors be found.
-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Analyze
  ( analyzeEllipsesCounts,
    analyzeEllipsesCaptures,
    analyzeEllipsesCapturesWithoutVariables,
    analyzeDefinitionSyntax,
    analyzePatternForMoreThan1EllipsisPerTerm,
    analyzeVariablesUsedMoreThanOnceInPattern,
  )
where

import Ast0 qualified
import Ast1 qualified
import AstC0 qualified
import CompileTypes (VariableBindings)
import Control.Comonad.Cofree qualified as C
import Control.Comonad.Trans.Cofree (CofreeF (..))
import Data.Foldable.Extra (Foldable (foldl'))
import Data.Functor.Foldable (Recursive (..))
import Data.HashMap.Strict qualified as H
import Data.Maybe (catMaybes, fromJust, listToMaybe, mapMaybe)
import Error
  ( badEllipsesCapturesErrorMessage,
    badEllipsesCountErrorMessage,
    definitionDoesNotStartWithDefErrorMessage,
    definitionHasWrongNumberOfTermsErrorMessage,
    expectedDefinitionGotSymbolErrorMessage,
    moreThanOneEllipsisInSingleTermOfPatternErrorMessage,
    noVariablesInEllipsisErrorMessage,
    variableUsedMoreThanOnceInPatternErrorMessage,
  )
import ErrorTypes (ErrorMessage, Span)
import ReadTypes (SrcLocked)
import Utils
  ( Between,
    Cata,
    Para,
    getPatternSpanAtC0Index,
    isDollarSignVar,
    popBetweenTail,
    popTrailingC1Index,
    pushBetweenTail,
  )
import Prelude hiding (span)

-- | Finds errors relating to the use of too few or too many ellipses on variables
analyzeEllipsesCounts :: VariableBindings -> SrcLocked AstC0.Ast -> [ErrorMessage]
analyzeEllipsesCounts variableBindings ast = cata go ast 0
  where
    go :: Cata (SrcLocked AstC0.Ast) (Int -> [ErrorMessage])
    go cofree actualEllipsesCount = case cofree of
      _ :< AstC0.SymbolF _ -> []
      _ :< AstC0.CompoundF xs -> concatMap ($ actualEllipsesCount) xs
      constructorVarSpan :< AstC0.VariableF x s ->
        let requiredCount = requiredEllipses x
            (_, patternVarSpan) = fromJust $ variableBindings H.!? s
            errorMessage =
              badEllipsesCountErrorMessage
                requiredCount
                actualEllipsesCount
                patternVarSpan
                constructorVarSpan
         in [errorMessage | requiredCount /= actualEllipsesCount]
      _ :< AstC0.EllipsesF x -> x $ actualEllipsesCount + 1
    requiredEllipses :: AstC0.Index -> Int
    requiredEllipses = length . filter AstC0.isBetween

-- | Finds errors relating to the use of variables under the same ellipsis that
-- weren't matched under the same ellipsis.
--
-- For example, '(def ((x ..) (y ..)) ((x y) ..))' is detected as erroneous here. The
-- problem with this example is that the number of terms matched to 'x' may be different
-- from the number of terms matched to 'y', so it is not in general possible to create
-- '(x y) ..'.
analyzeEllipsesCaptures :: SrcLocked Ast1.Ast -> SrcLocked AstC0.Ast -> [ErrorMessage]
analyzeEllipsesCaptures pattern = fixup . cata go
  where
    fixup = \case
      Left errors -> errors
      Right _ -> []
    go :: Cata (SrcLocked AstC0.Ast) (Either [ErrorMessage] [Assignment])
    go (span :< ast) = case ast of
      AstC0.SymbolF _ -> Right []
      AstC0.CompoundF xs -> concat <$> sequence xs
      AstC0.EllipsesF eithers -> do
        assignments <- eithers
        case listToMaybe assignments of
          Nothing -> Right []
          Just firstAssignment ->
            let checkAssignment :: Assignment -> Either [ErrorMessage] (Maybe Assignment)
                checkAssignment a =
                  if firstAssignment.index == a.index
                    then
                      let index = fst a.index
                          (c0, _c1) = popTrailingC1Index index
                          (c0', maybeBetween) = popBetweenTail c0
                       in case maybeBetween of
                            Nothing -> Right Nothing
                            Just between ->
                              Right . Just $ a {index = (c0', between)}
                    else
                      let var1Name = firstAssignment.variableName
                          ellipses1PatternSpan =
                            fromJust $
                              getPatternSpanAtC0Index pattern $
                                pushBetweenTail firstAssignment.index
                          var2Name = a.variableName
                          ellipses2PatternSpan =
                            fromJust $
                              getPatternSpanAtC0Index pattern $
                                pushBetweenTail a.index
                          ellipsesConstructorSpan = span
                       in Left
                            [ badEllipsesCapturesErrorMessage
                                var1Name
                                ellipses1PatternSpan
                                var2Name
                                ellipses2PatternSpan
                                ellipsesConstructorSpan
                            ]
             in catMaybes <$> mapM checkAssignment assignments
      AstC0.VariableF index variableName ->
        let (c0, _c1) = popTrailingC1Index index
            (c0', maybeBetween) = popBetweenTail c0
         in Right $ case maybeBetween of
              Nothing -> []
              Just between ->
                [ Assignment
                    { variableName,
                      variableSpan = span,
                      index = (c0', between)
                    }
                ]

data Assignment = Assignment
  { variableName :: String,
    variableSpan :: Span Int,
    index :: (AstC0.Index, Between)
  }
  deriving (Show)

type HasVariable = Bool

-- | Finds errors of ellipses capturing no variables
analyzeEllipsesCapturesWithoutVariables :: SrcLocked Ast1.Ast -> [ErrorMessage]
analyzeEllipsesCapturesWithoutVariables = fixup . para go
  where
    fixup = \case
      Left errors -> errors
      Right _ -> []
    -- 'para' is used here instead of 'cata' solely to get access to the
    -- source location (span) of the term containing no variables inside
    -- an ellipsis, should one be found to exist.
    go :: Para (SrcLocked Ast1.Ast) (Either [ErrorMessage] HasVariable)
    go (span :< ast) = case ast of
      Ast1.SymbolF s -> Right $ isDollarSignVar s
      Ast1.CompoundF inputResultPairs -> do
        let results :: [Either [ErrorMessage] HasVariable]
            results = map snd inputResultPairs
        hasVariableList <- sequence results
        Right $ or hasVariableList
      Ast1.EllipsesF x -> do
        let -- accessing 'originalInputSpan' is the only reason we need
            -- to use 'para' instead of 'cata'
            (originalInputSpan C.:< _originalInput) = fst x
            result :: Either [ErrorMessage] HasVariable
            result = snd x
        hasVariable <- result
        if hasVariable
          then Right True
          else Left [noVariablesInEllipsisErrorMessage span originalInputSpan]

-- | Finds problems relating to definitions either (1) being symbols, not definitions;
-- (2) not having 3 terms; (3) not starting with the symbol 'def'.
analyzeDefinitionSyntax :: SrcLocked Ast0.Ast -> [ErrorMessage]
analyzeDefinitionSyntax (span C.:< definition) = case definition of
  Ast0.SymbolF _s ->
    [expectedDefinitionGotSymbolErrorMessage span]
  Ast0.CompoundF xs ->
    if length xs /= 3
      then [definitionHasWrongNumberOfTermsErrorMessage span $ length xs]
      else
        let notDefSymbol :: SrcLocked Ast0.Ast -> Maybe (Span Int)
            notDefSymbol (_ C.:< Ast0.SymbolF "def") = Nothing
            notDefSymbol (otherThanDefSpan C.:< _) = Just otherThanDefSpan
         in case notDefSymbol $ head xs of
              Nothing -> []
              Just otherThanDefSpan ->
                [definitionDoesNotStartWithDefErrorMessage otherThanDefSpan]

-- | Finds errors relating to the use of more than one ellipsis per term in a definition's
-- pattern. For exmaple '($a .. $b ..)' is detected here.
analyzePatternForMoreThan1EllipsisPerTerm :: SrcLocked Ast1.Ast -> [ErrorMessage]
analyzePatternForMoreThan1EllipsisPerTerm = para go
  where
    go :: Para (SrcLocked Ast1.Ast) [ErrorMessage]
    go (span :< ast) = case ast of
      Ast1.SymbolF _s -> []
      Ast1.CompoundF inputResultPairs -> do
        let badInputResultPairs = filter (not . null . snd) inputResultPairs
        if null badInputResultPairs
          then
            let inputs :: [SrcLocked Ast1.Ast]
                inputs = map fst inputResultPairs

                ellipsisSpan :: SrcLocked Ast1.Ast -> Maybe (Span Int)
                ellipsisSpan (span C.:< ast) = case ast of
                  Ast1.EllipsesF _ -> Just span
                  _ -> Nothing

                ellipsesInputSpans :: [Span Int]
                ellipsesInputSpans = mapMaybe ellipsisSpan inputs

                errorMessage :: ErrorMessage
                errorMessage =
                  moreThanOneEllipsisInSingleTermOfPatternErrorMessage
                    span
                    $ drop 1 ellipsesInputSpans
             in ([errorMessage | length ellipsesInputSpans > 1])
          else
            let errors :: [ErrorMessage]
                errors = concatMap snd badInputResultPairs
             in errors
      Ast1.EllipsesF (_input, result) -> result

-- | Finds errors relating to the use of a variable twice or more in the pattern
-- of a definition.
analyzeVariablesUsedMoreThanOnceInPattern :: SrcLocked Ast1.Ast -> [ErrorMessage]
analyzeVariablesUsedMoreThanOnceInPattern = report . cata findVarSpans
  where
    report :: H.HashMap String [Span Int] -> [ErrorMessage]
    report uses =
      let spansOfVarsWithMoreThanOneUse :: [[Span Int]]
          spansOfVarsWithMoreThanOneUse = filter ((> 1) . length) $ H.elems uses
       in map
            variableUsedMoreThanOnceInPatternErrorMessage
            spansOfVarsWithMoreThanOneUse

    findVarSpans :: Cata (SrcLocked Ast1.Ast) (H.HashMap String [Span Int])
    findVarSpans (span :< ast) = case ast of
      Ast1.SymbolF s ->
        if isDollarSignVar s
          then H.insert s [span] H.empty
          else H.empty
      Ast1.CompoundF xs -> foldl' (H.unionWith (++)) H.empty xs
      Ast1.EllipsesF x -> x