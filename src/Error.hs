module Error
  ( ErrorType (..),
    CompileResult,
    ErrorMessageInfo (..),
    formatErrorMessage,
    Span (..),
    SrcMap (..),
    badEllipsesCount,
    genericErrorInfo,
  )
where

import AstP0 qualified
import Data.HashMap.Strict qualified as H
import Data.List.Extra qualified as L
import Data.Text (Text, pack)
import Data.Text qualified as T
import Text.Parsec (ParseError)
import Text.Parsec.Error (errorMessages, errorPos, messageString)
import Text.Parsec.Pos (SourcePos, sourceColumn, sourceLine)
import Utils (tshow)

type CompileResult a = Either ErrorMessageInfo a

data ErrorType
  = ParsingError
  | BadEllipsesCount
  | VarsNotCapturedUnderSameEllipsisInConstructor
  | EllipsisAppliedToSymbolInConstructor
  | InvalidRuleDefinition
  | MoreThanOneEllipsisInSingleCompoundTermOfPattern
  | VariableUsedMoreThanOnceInPattern
  | OverlappingPatterns
  deriving (Eq, Show)

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

-- sourcePosToSrcloc :: Filename -> SourcePos -> Srcloc
-- sourcePosToSrcloc filename sourcePos =
--   Srcloc
--     { source = filename,
--       line = sourceLine sourcePos,
--       column = sourceColumn sourcePos
--     }

-- parsingErrorInfo :: ParseError -> Filename -> ErrorMessageInfo
-- parsingErrorInfo parseError source =
--   ErrorMessageInfo
--     { errorType = ParsingError,
--       message = "bad syntax",
--       sourceSnippets =
--         [ SrcSnippet
--             { srcloc = srcloc,
--               annotation =
--                 SrcSnippetAnnotation
--                   { columnStart = column srcloc,
--                     columnEnd = column srcloc + 1,
--                     text = T.concat (map (tshow . messageString) $ errorMessages parseError)
--                   }
--             }
--         ],
--       help = Nothing
--     }
--   where
--     srcloc = sourcePosToSrcloc source (errorPos parseError)

genericErrorInfo :: ErrorType -> ErrorMessageInfo
genericErrorInfo errorType =
  ErrorMessageInfo
    { errorType = InvalidRuleDefinition,
      message = "error message not yet implemented for " <> tshow errorType,
      annotations = [],
      help = Nothing
    }

badEllipsesCount :: Int -> Int -> Span -> Span -> ErrorMessageInfo
badEllipsesCount requiredCount actualCount patternVarSpan constructorVarSpan =
  ErrorMessageInfo
    { errorType = BadEllipsesCount,
      message = "too few ellipses",
      annotations =
        [ Annotation
            { span = constructorVarSpan,
              annotation = "used with " <> tshow actualCount <> " ellipses here"
            },
          Annotation
            { span = patternVarSpan,
              annotation = "used with " <> tshow requiredCount <> " ellipses here"
            }
        ],
      help = Just "variables must be used with the same number of ellipses in the pattern and constructor"
    }

-- data TooFewEllipsesInfo = TooFewEllipsesInfo
--   {
--   }

data ErrorMessageInfo = ErrorMessageInfo
  { errorType :: ErrorType,
    message :: Text,
    annotations :: [Annotation],
    help :: Maybe Text
  }
  deriving (Eq, Show)

data Annotation = Annotation
  { span :: Span,
    annotation :: Text
  }
  deriving (Eq, Show)

data Span = Span
  { source :: Filename,
    start :: Pos,
    end :: Pos
  }
  deriving (Eq, Show)

checkSpanInvariants :: Span -> ()
checkSpanInvariants span =
  case (spanPosBothValid span, spanEndIsPastStart span) of
    (True, True) -> ()
    (False, _) -> error "invariant broken: Span Pos both valid"
    (_, False) -> error "invariant broken: Span end is past start"

spanPosBothValid :: Span -> Bool
spanPosBothValid (Span {start, end}) = posHasNonnegativeFields start && posHasNonnegativeFields end

spanEndIsPastStart :: Span -> Bool
spanEndIsPastStart Span {start, end} = line start < line end || column start < column end

data Pos = Pos
  { line :: Int,
    column :: Int
  }
  deriving (Eq, Show)

checkPosInvariants :: Pos -> ()
checkPosInvariants pos =
  if posHasNonnegativeFields pos
    then ()
    else error "invariant broken: Pos has negative fields"

posHasNonnegativeFields :: Pos -> Bool
posHasNonnegativeFields (Pos {line, column}) = line >= 0 && column >= 0

type Filename = Text

type FileContents = Text

newtype SrcMap = SrcMap (H.HashMap Filename FileContents)

lookupPosLineText :: SrcMap -> Filename -> Pos -> Text
lookupPosLineText (SrcMap srcmap) source pos =
  let contents :: Text
      contents = case srcmap H.!? source of
        Nothing -> error "invalid filename"
        Just c -> c
      lineText :: Text
      lineText = case T.lines contents L.!? line pos of
        Nothing -> error "invalid pos line"
        Just l -> l
   in lineText

-- Returns Text resembling the following, an informative message about
-- a compiler error:
--
--   error[E1]: variable not matched in pattern used in constructor
--   source:line:col
--      |
--   10 | (def foo $x)
--      |          ^^ the variable
--   help: either delete `$x` or also use it in the pattern
--
--   For further information about this error, try `rw --explain E1`.
formatErrorMessage :: SrcMap -> Text -> ErrorMessageInfo -> Text
formatErrorMessage srcmap thisCompilerName (ErrorMessageInfo {errorType, message, annotations, help}) =
  "error["
    <> errorCodeText
    <> "]: "
    <> message
    <> "\n"
    <> formatSrcSnippets srcmap annotations
    <> ( case help of
           Nothing -> ""
           Just helpText -> "help: " <> helpText
       )
    <> "\n"
    <> "For further information about this error, try `"
    <> thisCompilerName
    <> " --explain "
    <> errorCodeText
    <> "`"
    <> "\n"
  where
    errorCodeText = "E" <> T.justifyRight 3 '0' (tshow $ errorCode errorType)

formatSrcSnippets :: SrcMap -> [Annotation] -> Text
formatSrcSnippets srcmap = T.unlines . map (formatSrcSnippet srcmap)

-- Produces text which resembles the following, describing
-- a portion of the text file that an error message applies to:
--
--   rules.txt:10:12
--      |
--   10 | (def mydef $x)
--      |            ^^ the variable
formatSrcSnippet :: SrcMap -> Annotation -> Text
formatSrcSnippet srcmap annotation =
  formatSrcloc annotation <> "\n" <> formatAnnotationBlock srcmap annotation

-- Produces text which labels the input name, row, and column,
-- separated by colons. Like this:
--
--   rules.txt:10:12
formatSrcloc :: Annotation -> Text
formatSrcloc (Annotation {span}) =
  T.intercalate
    ":"
    [ source span,
      tshow . line $ start span,
      tshow . column $ start span
    ]

-- Produces text which describes the location of an error in
-- the source code along with a annotation:
--
--      |
--   10 | (def mydef $x)
--      |            ^^ the variable
formatAnnotationBlock :: SrcMap -> Annotation -> Text
formatAnnotationBlock srcmap (Annotation {span = span@(Span {source, start}), annotation}) =
  T.unlines [line1, line2, line3]
  where
    line1 = T.replicate line1And3PaddingCount " " <> "|"
    line2 = " " <> columnNumberText <> " | " <> lineText
    line3 =
      line1
        <> " "
        <> T.replicate (column start) " "
        <> T.replicate numberOfCarrots "^"
        <> " "
        <> annotation

    lineText :: Text
    lineText = lookupPosLineText srcmap source start

    columnNumberText :: Text
    columnNumberText = tshow $ column start

    line1And3PaddingCount :: Int
    line1And3PaddingCount = T.length columnNumberText + 2

    numberOfCarrots :: Int
    numberOfCarrots = oneLineLengthSpanned span lineText

oneLineLengthSpanned :: Span -> Text -> Int
oneLineLengthSpanned span@(Span {start, end}) lineText =
  let result =
        seq
          (checkSpanInvariants span)
          ( if line start == line end
              then column end - column start
              else T.length lineText - column start
          )
   in if result < 0
        then error "span was outside of lineText"
        else result