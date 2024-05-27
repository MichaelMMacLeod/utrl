{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Error
  ( ErrorType (..),
    ErrorBundle (..),
    CompileResult,
    FileContents,
    OffendingLine (..),
    parseErrorMessage,
    genericErrorInfo,
    badEllipsesCountErrorMessage,
    formatErrorMessage,
    errorMessages,
    addLength,
    mkFilePathName,
    badEllipsesCapturesErrorMessage,
  )
where

import Data.Functor.Foldable (ListF (..), Recursive (..))
import Data.HashMap.Strict qualified as H
import Data.List (sortBy)
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
import GHC.Base (compareInt)
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
import Utils (Cata, tshow)
import Prelude hiding (span)

errorMessages :: Maybe FilePath -> FileContents -> [ErrorMessageInfo Int] -> Text
errorMessages name contents errors =
  T.concat . map formatErrorMessage $ errorBundleMessages bundle
  where
    bundle = mkErrorBundle name' contents errors
    name' = mkFilePathName name

mkFilePathName :: Maybe FilePath -> String
mkFilePathName = fromMaybe "<input>"

type CompileResult a = Either [ErrorMessageInfo Int] a

errorCode :: ErrorType -> Int
errorCode = \case
  ParsingError -> 1
  BadEllipsesCount -> 2
  VarsNotCapturedUnderSameEllipsisInConstructor -> 3
  EllipsisAppliedToSymbolInConstructor -> 4
  InvalidRuleDefinition -> 5
  MoreThanOneEllipsisInSingleCompoundTermOfPattern -> 6
  VariableUsedMoreThanOnceInPattern -> 7
  OverlappingPatterns -> 8

type FileContents = Text

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
    -- no choice but to sort them so that they are in order.
    sortedSpans :: [Span Int]
    sortedSpans = sortBy compareSpan spans

    compareSpan :: Span Int -> Span Int -> Ordering
    compareSpan s1 s2 = 
      -- You'd think the order would be s1.location and then s2.location, but we 
      -- reverse it here so that the earliest span comes last. This is because 'cata' 
      -- is a right-fold, i.e., it processes the last element in the list first.
      compareInt s2.location s1.location

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

data OffendingLine = OffendingLine
  { line :: Text,
    sourcePos :: SourcePos
  }
  deriving (Show)

formatErrorMessage :: ErrorMessageInfo OffendingLine -> Text
formatErrorMessage (ErrorMessageInfo {errorType, message, annotations, help}) =
  let errorCodeText = "E" <> T.justifyRight 3 '0' (tshow $ errorCode errorType)
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
    [pack sourceName, tshow $ unPos sourceLine, tshow $ unPos sourceColumn]

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
      lineNumberText = tshow . unPos $ sourceLine sourcePos

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

genericErrorInfo :: ErrorType -> [ErrorMessageInfo Int]
genericErrorInfo errorType =
  [ ErrorMessageInfo
      { errorType = InvalidRuleDefinition,
        message = "error message not yet implemented for " <> tshow errorType,
        annotations = [],
        help = Nothing
      }
  ]

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
      n -> tshow n <> " ellipses"

badEllipsesCapturesErrorMessage ::
  String ->
  Span Int ->
  String ->
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
                annotation = pack var1Name <> " matched under this ellipsis"
              },
            Annotation
              { span = ellipses2PatternSpan,
                annotation = pack var2Name <> " matched under this ellipsis"
              },
            Annotation
              { span = ellipsesConstructorSpan,
                annotation = "both used with this ellipsis"
              }
          ],
        help = Just "variables matched under different ellipses can't be used with the same ellipsis"
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