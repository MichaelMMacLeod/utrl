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
    ruleDefinitionPredicates,
    analyzeOverlappingPatterns,
    analyzeEllipsesAppliedToSymbols,
    unreachableBecauseOfAnalysisStep,
    analyzeVariableNotMatchedInPattern,
  )
where

import Ast0 qualified
import Ast1 qualified
import AstC0 qualified
import AstP0 (indexP0ByC0)
import AstP0 qualified
import CompileTypes (VariableBindings)
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Cofree qualified as C
import Control.Comonad.Trans.Cofree (CofreeF (..))
import Control.Monad (guard)
import Data.Foldable.Extra (Foldable (foldl'))
import Data.Functor.Foldable (Recursive (..))
import Data.HashMap.Strict qualified as H
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromJust, listToMaybe, mapMaybe)
import Data.Set qualified as S
import Error
  ( badEllipsesCapturesErrorMessage,
    badEllipsesCountErrorMessage,
    definitionDoesNotStartWithDefErrorMessage,
    definitionHasWrongNumberOfTermsErrorMessage,
    ellipsisAppliedToSymbolErrorMessage,
    expectedDefinitionGotSymbolErrorMessage,
    moreThanOneEllipsisInSingleTermOfPatternErrorMessage,
    noVariablesInEllipsisErrorMessage,
    overlappingPatternsErrorMessage,
    variableNotMatchedInPatternErrorMessage,
    variableUsedMoreThanOnceInPatternErrorMessage,
  )
import ErrorTypes (ErrorMessage, Span, location)
import Predicate
  ( IndexedPredicate (..),
    Predicate (..),
    applyPredicatesForOverlappingPatternAnalysis,
  )
import ReadTypes (SrcLocked)
import Utils
  ( Between,
    Cata,
    Para,
    compareSpan,
    getPatternSpanAtC0Index,
    isDollarSignVar,
    popBetweenTail,
    popTrailingC1Index,
    pushBetweenTail,
  )
import Prelude hiding (span)

-- | Finds errors relating to the use of a '$variable' in the constructor that was
-- not matched in the pattern, as in '(def (copy x) $x)'.
analyzeVariableNotMatchedInPattern :: SrcLocked Ast1.Ast -> SrcLocked Ast1.Ast -> [ErrorMessage]
analyzeVariableNotMatchedInPattern patternAst constructorAst =
  let variablesMatchedInPattern :: S.Set String
      variablesMatchedInPattern = cata goPattern patternAst

      goPattern :: Cata (SrcLocked Ast1.Ast) (S.Set String)
      goPattern (_span :< ast) = case ast of
        Ast1.SymbolF s ->
          if isDollarSignVar s
            then S.singleton s
            else S.empty
        Ast1.CompoundF xs -> S.unions xs
        Ast1.EllipsesF x -> x

      variablesNotMatchedInPatternSpans :: [Span Int]
      variablesNotMatchedInPatternSpans = cata goConstructor constructorAst

      goConstructor :: Cata (SrcLocked Ast1.Ast) [Span Int]
      goConstructor (span :< ast) = case ast of
        Ast1.SymbolF s ->
          let isVarNotMatchedInPattern =
                isDollarSignVar s
                  && not (S.member s variablesMatchedInPattern)
           in [span | isVarNotMatchedInPattern]
        Ast1.CompoundF xs -> concat xs
        Ast1.EllipsesF x -> x

      mkError :: Span Int -> ErrorMessage
      mkError varNotMatchedInPatternSpan =
        variableNotMatchedInPatternErrorMessage
          varNotMatchedInPatternSpan
          $ extract patternAst
   in map mkError variablesNotMatchedInPatternSpans

-- | Finds errors relating to the use of too few or too many ellipses on variables
analyzeEllipsesCounts :: VariableBindings -> SrcLocked AstC0.Ast -> [ErrorMessage]
analyzeEllipsesCounts variableBindings ast = cata go ast 0
  where
    go :: Cata (SrcLocked AstC0.Ast) (Int -> [ErrorMessage])
    go (span :< ast) actualEllipsesCount = case ast of
      AstC0.SymbolF _ -> []
      AstC0.CompoundF xs -> concatMap ($ actualEllipsesCount) xs
      AstC0.VariableF x s ->
        let requiredCount = requiredEllipses x
            (_, patternVarSpan) = fromJust $ variableBindings H.!? s
            errorMessage =
              badEllipsesCountErrorMessage
                requiredCount
                actualEllipsesCount
                patternVarSpan
                span
         in [errorMessage | requiredCount /= actualEllipsesCount]
      AstC0.EllipsesF x -> x $ actualEllipsesCount + 1
    requiredEllipses :: AstC0.Index -> Int
    requiredEllipses = length . filter AstC0.isBetween

-- | Finds errors relating to the use of variables under the same ellipsis that
-- weren't matched under the same ellipsis.
--
-- For example, '(def (($x ..) ($y ..)) (($x $y) ..))' is detected as erroneous here. The
-- problem with this example is that the number of terms matched to 'x' may be different
-- from the number of terms matched to 'y', so it is not in general possible to create
-- '(x y) ..'.
analyzeEllipsesCaptures :: SrcLocked Ast1.Ast -> SrcLocked AstC0.Ast -> [ErrorMessage]
analyzeEllipsesCaptures pattern = extractErrors . cata go
  where
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
                      index = (c0', between)
                    }
                ]

data Assignment = Assignment
  { variableName :: String,
    index :: (AstC0.Index, Between)
  }

type HasVariable = Bool

-- | Finds errors of ellipses applied to symbols. We detect this case
-- specifically so that a nice error message can be emitted suggesting
-- to prefix the symbol with a dollar-sign as to make it a variable.
analyzeEllipsesAppliedToSymbols :: SrcLocked Ast1.Ast -> [ErrorMessage]
analyzeEllipsesAppliedToSymbols = para go
  where
    go :: Para (SrcLocked Ast1.Ast) [ErrorMessage]
    go (_span :< ast) = case ast of
      Ast1.SymbolF _s -> []
      Ast1.CompoundF inputResultPairs -> concatMap snd inputResultPairs
      Ast1.EllipsesF (inputSpan C.:< input, result) -> case input of
        Ast1.SymbolF s
          | not $ isDollarSignVar s ->
              let error = ellipsisAppliedToSymbolErrorMessage s inputSpan
               in error : result
        _ -> result

-- | Finds errors of ellipses capturing no variables
analyzeEllipsesCapturesWithoutVariables :: SrcLocked Ast1.Ast -> [ErrorMessage]
analyzeEllipsesCapturesWithoutVariables = extractErrors . para go
  where
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

-- | Finds errors relating to the definition of two patterns that could
-- possibly match the same term.
--
-- For example, both of these definitions could match '(A B)'
--
-- > (def (A $x .. B) C)
-- >
-- > (def (A $x ..)   C)
--
-- The input to this fuction is the list of every pattern in a definition
-- file.
analyzeOverlappingPatterns :: [(VariableBindings, SrcLocked AstP0.Ast)] -> [ErrorMessage]
analyzeOverlappingPatterns =
  map mkErrorMessages . sortAndSimplify . mapMaybe analyze . createAnalysisInfo
  where
    mkErrorMessages :: OverlappingPatternSpans -> ErrorMessage
    mkErrorMessages x = overlappingPatternsErrorMessage x.pattern1Span x.pattern2Span

    sortAndSimplify :: [OverlappingPatternSpans] -> [OverlappingPatternSpans]
    sortAndSimplify xs =
      let simplifiedMap :: H.HashMap (Int, Int) OverlappingPatternSpans
          simplifiedMap = foldl' insertBySpans H.empty xs

          simplifiedList :: [OverlappingPatternSpans]
          simplifiedList = H.elems simplifiedMap

          sortedSimplifiedList :: [OverlappingPatternSpans]
          sortedSimplifiedList =
            sortBy
              compareOverlappingPatternSpans
              simplifiedList

          compareOverlappingPatternSpans ::
            OverlappingPatternSpans -> OverlappingPatternSpans -> Ordering
          compareOverlappingPatternSpans o1 o2 =
            case compareSpan o1.pattern1Span o2.pattern1Span of
              EQ -> compareSpan o1.pattern2Span o2.pattern2Span
              ordering -> ordering

          insertBySpans ::
            H.HashMap (Int, Int) OverlappingPatternSpans ->
            OverlappingPatternSpans ->
            H.HashMap (Int, Int) OverlappingPatternSpans
          insertBySpans acc spans =
            let x = min spans.pattern1Span.location spans.pattern2Span.location
                y = max spans.pattern1Span.location spans.pattern2Span.location
             in if H.member (x, y) acc
                  then acc
                  else H.insert (x, y) spans acc
       in sortedSimplifiedList

    analyze :: OverlappingPatternAnalysisInfo -> Maybe OverlappingPatternSpans
    analyze info =
      let overlappingPatternsDetected =
            applyPredicatesForOverlappingPatternAnalysis
              info.pattern1Predicates
              info.pattern2WithEllipsesRemoved
       in if overlappingPatternsDetected
            then
              Just
                OverlappingPatternSpans
                  { pattern1Span = info.pattern1Span,
                    pattern2Span = info.pattern2Span
                  }
            else Nothing

    createAnalysisInfo :: [(VariableBindings, SrcLocked AstP0.Ast)] -> [OverlappingPatternAnalysisInfo]
    createAnalysisInfo patterns = do
      let indexedPatterns :: [(Int, (VariableBindings, SrcLocked AstP0.Ast))]
          indexedPatterns = zip [0 ..] patterns
      (index1, (pattern1VariableBindings, pattern1@(pattern1Span C.:< _))) <- indexedPatterns
      (index2, (_patternVariableBindings, pattern2@(pattern2Span C.:< _))) <- indexedPatterns
      guard (index1 /= index2) -- every pattern overlaps itself
      let pattern1Predicates = ruleDefinitionPredicates pattern1VariableBindings pattern1
          pattern2WithEllipsesRemoved = removeEllipsesFromPattern pattern2
      pure
        OverlappingPatternAnalysisInfo
          { pattern1Span,
            pattern1Predicates,
            pattern2Span,
            pattern2WithEllipsesRemoved
          }

data OverlappingPatternAnalysisInfo = OverlappingPatternAnalysisInfo
  { pattern1Span :: Span Int,
    pattern1Predicates :: [IndexedPredicate],
    pattern2Span :: Span Int,
    pattern2WithEllipsesRemoved :: Ast0.Ast
  }

data OverlappingPatternSpans = OverlappingPatternSpans
  { pattern1Span :: Span Int,
    pattern2Span :: Span Int
  }

-- | Removes the ellipses, and any terms inside of ellipses, from a pattern.
--
-- This is used when checking if, for example, these two definitions could
-- match the same term:
--
-- > (def (x $x ..) _)
--
-- > (def (x $x .. y) _)
--
-- To do this, we take the predicates corresponding to the first pattern
-- and apply them to '(x y)', i.e., to the second definition's pattern
-- with its ellipses (and the $x in the ellipses) removed. Then we do the
-- same thing, but apply the second definition's predicates to the first's
-- ellipses-removed-pattern.
--
-- In this case, these rule are overlapping, as both match the term '(x y)'.
removeEllipsesFromPattern :: SrcLocked AstP0.Ast -> Ast0.Ast
removeEllipsesFromPattern = cata go
  where
    go :: Cata (SrcLocked AstP0.Ast) Ast0.Ast
    go (_ :< ast) = case ast of
      AstP0.SymbolF s -> Ast0.Symbol s
      AstP0.CompoundWithoutEllipsesF xs -> Ast0.Compound xs
      AstP0.CompoundWithEllipsesF b e a -> Ast0.Compound $ b ++ [e] ++ a

-- Returns a list of conditions that must hold for a given rule's pattern to
-- match a term.
--
-- For example, in the following rule:
--
--   (def xs (flatten (list (list xs ..) ..)) -> (list xs .. ..))
-- Data.HashMap
-- the following conditions must hold if the rule is to match a given term:
--
-- - Index [] is a compound term of length == 2
-- - Index [0] == "flatten"
-- - Index [1] is a compound term of length >= 1
-- - Index [1,0] == "list"
-- - Indices [1,1..length] are compound terms of length >= 1
-- - Indices [1,1..length,0] == "list"
ruleDefinitionPredicates :: VariableBindings -> SrcLocked AstP0.Ast -> [IndexedPredicate]
ruleDefinitionPredicates vars pat = cata go (indexP0ByC0 pat)
  where
    go :: Cata (Cofree AstP0.AstF (Span Int, AstC0.Index)) [IndexedPredicate]
    go ((_l, index) :< ast) = case ast of
      AstP0.SymbolF s ->
        [IndexedPredicate (SymbolEqualTo s) index | not $ H.member s vars]
      AstP0.CompoundWithoutEllipsesF xs ->
        let xs' = concat xs
            p = IndexedPredicate (LengthEqualTo (length xs)) index
         in p : xs'
      AstP0.CompoundWithEllipsesF b e a ->
        let b' = concat b
            a' = concat a
            p = IndexedPredicate (LengthGreaterThanOrEqualTo $ length b + length a) index
         in p : (b' ++ e ++ a')

unreachableBecauseOfAnalysisStep :: String -> a
unreachableBecauseOfAnalysisStep step = error $ "analysis step '" <> step <> "' contract broken"

extractErrors :: Either [ErrorMessage] b -> [ErrorMessage]
extractErrors = \case
  Left errors -> errors
  Right _ -> []