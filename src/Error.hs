{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Error
  ( ErrorType (..),
    ErrorBundle (..),
    CompileResult (..),
    FileContents,
    OffendingLine (..),
    parseErrorMessage,
    badEllipsesCountErrorMessage,
    formatErrorMessage,
    errorMessages,
    addLength,
    mkFilePathName,
    badEllipsesCapturesErrorMessage,
    noVariablesInEllipsisErrorMessage,
    expectedDefinitionGotSymbolErrorMessage,
    definitionHasWrongNumberOfTermsErrorMessage,
    definitionDoesNotStartWithDefErrorMessage,
    moreThanOneEllipsisInSingleTermOfPatternErrorMessage,
    variableUsedMoreThanOnceInPatternErrorMessage,
    overlappingPatternsErrorMessage,
    ellipsisAppliedToSymbolErrorMessage,
    variableNotMatchedInPatternErrorMessage,
    emitStageMessage,
  )
where

import CompileTypes (Storage)
import Data.Functor.Foldable (ListF (..), Recursive (..))
import Data.HashMap.Strict qualified as H
import Data.Kind (Type)
import Data.List (intersperse, sortBy)
import Data.List.NonEmpty.Extra (toList)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Void (Void)
import ErrorTypes
  ( Annotation (..),
    ErrorMessage,
    ErrorMessageInfo (..),
    ErrorType (..),
    Span (..),
  )
import Text.Megaparsec
  ( ParseErrorBundle (..),
    PosState (..),
    SourcePos (..),
    TraversableStream (reachOffset),
    defaultTabWidth,
    errorOffset,
    initialPos,
    parseErrorTextPretty,
  )
import Text.Megaparsec.Error (ParseError)
import Text.Megaparsec.Pos (unPos)
import Utils (Cata, compareSpan, flipOrder, intToText)
import Prelude hiding (span)

errorMessages :: Maybe FilePath -> FileContents -> [ErrorMessageInfo Int] -> Text
errorMessages name contents errors =
  T.concat . intersperse "\n" . map formatErrorMessage $ errorBundleMessages bundle
  where
    bundle = mkErrorBundle name' contents errors
    name' = mkFilePathName name

mkFilePathName :: Maybe FilePath -> String
mkFilePathName = fromMaybe "<input>"

type CompileResult :: Type -> Type
data CompileResult a = CompileResult
  { storage :: Storage,
    result :: Either [ErrorMessageInfo Int] a
  }

errorCode :: ErrorType -> Int
errorCode = \case
  ParsingError -> 1
  BadEllipsesCount -> 2
  VarsNotCapturedUnderSameEllipsisInConstructor -> 3
  EllipsisAppliedToSymbol -> 4
  MoreThanOneEllipsisInSingleCompoundTermOfPattern -> 5
  VariableUsedMoreThanOnceInPattern -> 6
  OverlappingPatterns -> 7
  NoVariablesInEllipsis -> 8
  ExpectedDefinitionGotSymbol -> 9
  DefinitionHasWrongNumberOfTerms -> 10
  DefinitionDoesNotStartWithDef -> 11
  VariableNotMatchedInPattern -> 12
  EmitStageInfo -> 13

type FileContents :: Type
type FileContents = Text

type ErrorBundle :: Type
data ErrorBundle = ErrorBundle
  { posState :: PosState Text,
    errors :: [ErrorMessageInfo Int]
  }

mkErrorBundle :: FilePath -> FileContents -> [ErrorMessageInfo Int] -> ErrorBundle
mkErrorBundle name contents errors =
  ErrorBundle
    { posState = initialPosState name contents,
      errors
    }

errorBundleMessages :: ErrorBundle -> [ErrorMessageInfo OffendingLine]
errorBundleMessages errorBundle =
  let mapping = resolveSpans errorBundle.posState (errorBundleSpans errorBundle)
   in map (resolveErrorMessageInfoSpans mapping) errorBundle.errors

resolveErrorMessageInfoSpans ::
  H.HashMap (Span Int) (Span OffendingLine) ->
  ErrorMessageInfo Int ->
  ErrorMessageInfo OffendingLine
resolveErrorMessageInfoSpans mapping errorMessageInfo =
  let resolveAnnotationSpan :: Annotation Int -> Annotation OffendingLine
      resolveAnnotationSpan a = a {span = fromJust $ mapping H.!? a.span}
      annotations = map resolveAnnotationSpan errorMessageInfo.annotations
   in errorMessageInfo {annotations}

errorBundleSpans :: ErrorBundle -> [Span Int]
errorBundleSpans errorBundle = concatMap errorMessageInfoSpans errorBundle.errors

errorMessageInfoSpans :: ErrorMessageInfo Int -> [Span Int]
errorMessageInfoSpans errorMessageInfo =
  map (\annotaiton -> annotaiton.span) errorMessageInfo.annotations

resolveSpans :: PosState Text -> [Span Int] -> H.HashMap (Span Int) (Span OffendingLine)
resolveSpans posState spans =
  let (_posState', result) = cata go sortedSpans
   in result
  where
    -- 'resolveSpan' can't backtrack through the file to find an earlier span, we have
    -- no choice but to sort them so that they are in order. We flip the order because
    -- 'cata' processes the end of the list first.
    sortedSpans :: [Span Int]
    sortedSpans = sortBy (\s1 s2 -> flipOrder $ compareSpan s1 s2) spans

    go :: Cata [Span Int] (PosState Text, H.HashMap (Span Int) (Span OffendingLine))
    go = \case
      Nil -> (posState, H.empty)
      Cons span (posState, result) ->
        let (posState', span') = resolveSpan posState span
            result' = H.insert span span' result
         in (posState', result')

resolveSpan :: PosState Text -> Span Int -> (PosState Text, Span OffendingLine)
resolveSpan posState span =
  let (maybeLine, posState') = reachOffset span.location posState
      line = pack $ fromMaybe "" maybeLine
      sourcePos = pstateSourcePos posState'
      offendingLine = OffendingLine {line, sourcePos}
   in (posState', span {location = offendingLine})

-- Extends the first span to include the length of the second
addLength :: Span Int -> Span Int -> Span Int
addLength s1 s2 =
  Span
    { location = s1.location,
      length = s2.location - s1.location + s2.length
    }

type OffendingLine :: Type
data OffendingLine = OffendingLine
  { line :: Text,
    sourcePos :: SourcePos
  }
  deriving stock (Show)

formatErrorMessage :: ErrorMessageInfo OffendingLine -> Text
formatErrorMessage (ErrorMessageInfo {errorType, message, annotations, help}) =
  let errorCodeText = "E" <> T.justifyRight 3 '0' (intToText $ errorCode errorType)
   in "error["
        <> errorCodeText
        <> "]: "
        <> message
        <> "\n"
        <> formatAnnotations annotations
        <> ( case help of
               Nothing -> ""
               Just helpText -> "help: " <> helpText <> "\n"
           )

formatAnnotations :: [Annotation OffendingLine] -> Text
formatAnnotations = T.concat . map formatAnnotation

formatAnnotation :: Annotation OffendingLine -> Text
formatAnnotation a =
  formatSourcePos (sourcePos . location $ span a)
    <> "\n"
    <> formatAnnotationBlock a

formatSourcePos :: SourcePos -> Text
formatSourcePos (SourcePos {sourceName, sourceLine, sourceColumn}) =
  T.intercalate
    ":"
    [pack sourceName, intToText $ unPos sourceLine, intToText $ unPos sourceColumn]

formatAnnotationBlock :: Annotation OffendingLine -> Text
formatAnnotationBlock (Annotation {span, annotation}) =
  let Span {location, length} = span
      OffendingLine {line, sourcePos} = location
      columnZeroIndexed = unPos (sourceColumn sourcePos) - 1
      line1 = T.replicate line1And3PaddingCount " " <> "|"
      line2 = " " <> lineNumberText <> " | " <> line
      line3 =
        line1
          <> " "
          <> T.replicate columnZeroIndexed " "
          <> T.replicate length "^"
          <> " "
          <> indentedAnnotation
      line1And3PaddingCount = T.length lineNumberText + 2
      lineNumberText = intToText . unPos $ sourceLine sourcePos

      indentedAnnotation :: Text
      indentedAnnotation =
        let ls = T.lines annotation
         in case ls of
              [] -> error "unreachable: empty annotation"
              (l : others) ->
                let indentedOthers = map ((line1 <> T.replicate indentation " ") <>) others
                    indentation = 3 + columnZeroIndexed
                 in T.unlines $ l : indentedOthers
   in T.unlines $ map T.stripEnd [line1, line2, line3]

parseErrorMessage :: ParseErrorBundle Text Void -> ErrorMessage
parseErrorMessage (ParseErrorBundle {bundleErrors}) =
  ErrorMessageInfo
    { errorType = ParsingError,
      message = "bad syntax",
      annotations = map go $ toList bundleErrors,
      help = Nothing
    }
  where
    go :: ParseError Text Void -> Annotation Int
    go e =
      Annotation
        { span =
            Span
              { location = errorOffset e,
                -- it's impossible for us to generate a parse error with
                -- length > 1, so we can hardcode the answer here
                length = 1
              },
          annotation = pack $ parseErrorTextPretty e
        }

badEllipsesCountErrorMessage :: Int -> Int -> Span Int -> Span Int -> ErrorMessage
badEllipsesCountErrorMessage requiredCount actualCount patternVar constructorVar =
  ErrorMessageInfo
    { errorType = BadEllipsesCount,
      message =
        if actualCount < requiredCount
          then "too few ellipses"
          else
            if actualCount > requiredCount
              then "too many ellipses"
              else error "unreachable: actualCount = requiredCount",
      annotations =
        [ Annotation
            { span = patternVar,
              annotation = "matched with " <> numDotDotWords requiredCount <> " in pattern"
            },
          Annotation
            { span = constructorVar,
              annotation = "used with " <> numDotDotWords actualCount <> " in constructor"
            }
        ],
      help = Just "variables must be used with the same number of ellipses they were matched with"
    }
  where
    numDotDotWords :: Int -> Text
    numDotDotWords = \case
      1 -> "1 ellipsis"
      n -> intToText n <> " ellipses"

ellipsisAppliedToSymbolErrorMessage :: Text -> Span Int -> ErrorMessage
ellipsisAppliedToSymbolErrorMessage symbolName symbolSpan =
  ErrorMessageInfo
    { errorType = EllipsisAppliedToSymbol,
      message = "ellipsis following symbol",
      annotations =
        [ Annotation
            { span = symbolSpan,
              annotation =
                "this symbol doesn't begin with a dollar-sign ('$'),\n"
                  <> "so it is not considered a variable"
            }
        ],
      help = Just $ "perhaps you meant '$" <> symbolName <> "'?"
    }

badEllipsesCapturesErrorMessage ::
  Text ->
  Span Int ->
  Text ->
  Span Int ->
  Span Int ->
  ErrorMessage
badEllipsesCapturesErrorMessage
  var1Name
  ellipses1PatternSpan
  var2Name
  ellipses2PatternSpan
  ellipsesConstructorSpan =
    ErrorMessageInfo
      { errorType = VarsNotCapturedUnderSameEllipsisInConstructor,
        message = "variables matched under different ellipses used with same ellipsis",
        annotations =
          [ Annotation
              { span = ellipses1PatternSpan,
                annotation = var1Name <> " matched under this ellipsis"
              },
            Annotation
              { span = ellipses2PatternSpan,
                annotation = var2Name <> " matched under this ellipsis"
              },
            Annotation
              { span = ellipsesConstructorSpan,
                annotation = "both used with this ellipsis"
              }
          ],
        help = Just "variables matched under different ellipses can't be used with the same ellipsis"
      }

noVariablesInEllipsisErrorMessage :: Span Int -> Span Int -> ErrorMessage
noVariablesInEllipsisErrorMessage ellipsisSpan termWithNoVariablesSpan =
  ErrorMessageInfo
    { errorType = NoVariablesInEllipsis,
      message = "no variables in term preceding ellipsis",
      annotations =
        [ Annotation
            { span = ellipsisSpan,
              annotation = "the ellipsis"
            },
          Annotation
            { span = termWithNoVariablesSpan,
              annotation = "the term preceding the ellipsis"
            }
        ],
      help = Just "there must be at least one variable in the term preceding an ellipsis"
    }

badDefinitionHelp :: Maybe Text
badDefinitionHelp = Just "definitions look like this: '(def <pattern> <constructor>)'"

expectedDefinitionGotSymbolErrorMessage :: Span Int -> ErrorMessage
expectedDefinitionGotSymbolErrorMessage symbolSpan =
  ErrorMessageInfo
    { errorType = ExpectedDefinitionGotSymbol,
      message = "expected definition, found symbol",
      annotations =
        [ Annotation
            { span = symbolSpan,
              annotation = "this should be a definition"
            }
        ],
      help = badDefinitionHelp
    }

definitionHasWrongNumberOfTermsErrorMessage :: Span Int -> Int -> ErrorMessage
definitionHasWrongNumberOfTermsErrorMessage defSpan actualNumberOfTerms =
  ErrorMessageInfo
    { errorType = DefinitionHasWrongNumberOfTerms,
      message = case actualNumberOfTerms of
        n | n < 3 -> "definition has too few terms"
        n | n > 3 -> "definition has too many terms"
        _ -> error "unreachable: correct number of terms",
      annotations =
        [ Annotation
            { span = defSpan,
              annotation = "has " <> intToText actualNumberOfTerms <> " terms, but should have 3"
            }
        ],
      help = badDefinitionHelp
    }

definitionDoesNotStartWithDefErrorMessage :: Span Int -> ErrorMessage
definitionDoesNotStartWithDefErrorMessage otherThanDefSpan =
  ErrorMessageInfo
    { errorType = DefinitionDoesNotStartWithDef,
      message = "definition does not start with 'def'",
      annotations =
        [ Annotation
            { span = otherThanDefSpan,
              annotation = "this term should be 'def'"
            }
        ],
      help = badDefinitionHelp
    }

moreThanOneEllipsisInSingleTermOfPatternErrorMessage :: Span Int -> [Span Int] -> ErrorMessage
moreThanOneEllipsisInSingleTermOfPatternErrorMessage termWithEllipsesSpan extraEllipsesSpans =
  ErrorMessageInfo
    { errorType = MoreThanOneEllipsisInSingleCompoundTermOfPattern,
      message = "too many ellipses in single term of pattern",
      annotations =
        Annotation
          { span = termWithEllipsesSpan,
            annotation = "this term has more than one ellipsis"
          }
          : zipWith mkExtraEllipsisSpanAnnotation [2 ..] extraEllipsesSpans,
      help = Just "each term in a definitions's pattern may have at most one ellipsis"
    }
  where
    mkExtraEllipsisSpanAnnotation :: Int -> Span Int -> Annotation Int
    mkExtraEllipsisSpanAnnotation ellipsisNumber ellipsisSpan =
      Annotation
        { span = ellipsisSpan,
          annotation = "ellipsis #" <> intToText ellipsisNumber
        }

variableUsedMoreThanOnceInPatternErrorMessage :: [Span Int] -> ErrorMessage
variableUsedMoreThanOnceInPatternErrorMessage varUseSpans =
  ErrorMessageInfo
    { errorType = VariableUsedMoreThanOnceInPattern,
      message = "variable occurs more than once in pattern",
      annotations = zipWith mkAnnotation [1 ..] varUseSpans,
      help = Just "a variable may occur at most once in a definition's pattern"
    }
  where
    mkAnnotation :: Int -> Span Int -> Annotation Int
    mkAnnotation useNumber span =
      Annotation
        { span,
          annotation = "use #" <> intToText useNumber
        }

overlappingPatternsErrorMessage :: Span Int -> Span Int -> ErrorMessage
overlappingPatternsErrorMessage pattern1Span pattern2Span =
  ErrorMessageInfo
    { errorType = OverlappingPatterns,
      message = "overlapping patterns",
      annotations =
        [ Annotation
            { span = pattern1Span,
              annotation = "this pattern may match the same term as ..."
            },
          Annotation
            { span = pattern2Span,
              annotation = "... this other pattern"
            }
        ],
      help = Just "patterns possibly matching the same term are not allowed"
    }

variableNotMatchedInPatternErrorMessage :: Span Int -> Span Int -> ErrorMessage
variableNotMatchedInPatternErrorMessage variableSpan patternSpan =
  ErrorMessageInfo
    { errorType = VariableNotMatchedInPattern,
      message = "variable not matched in pattern",
      annotations =
        [ Annotation
            { span = variableSpan,
              annotation = "the variable"
            },
          Annotation
            { span = patternSpan,
              annotation = "the pattern"
            }
        ],
      help = Just "variables cannot be used without first being matched"
    }

emitStageMessage :: Span Int -> Text -> Text -> ErrorMessage
emitStageMessage span emitStage astText =
  ErrorMessageInfo
    { errorType = EmitStageInfo,
      message = "[--emit] generated code for stage " <> emitStage,
      annotations =
        [ Annotation
            { span, annotation = astText }
        ],
      help = Nothing
    }

-- Copied from megaparsec 9.6.1 as our version here isn't high enough yet for
-- this to be defined.
initialPosState ::
  -- | Name of the file the input is coming from
  FilePath ->
  -- | Input
  s ->
  PosState s
initialPosState name s =
  PosState
    { pstateInput = s,
      pstateOffset = 0,
      pstateSourcePos = initialPos name,
      pstateTabWidth = defaultTabWidth,
      pstateLinePrefix = ""
    }