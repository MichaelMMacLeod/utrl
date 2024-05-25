module Error
  ( ErrorType (..),
    ErrorBundle (..),
    CompileResult,
    FileContents,
    extractErorrType,
    OffendingLine (..),
    parseErrorMessage,
    genericErrorInfo,
    badEllipsesCountErrorMessage,
    formatErrorMessage,
    errorMessages,
    addLength,
    mkFilePathName,
  )
where

import Data.Either.Extra (mapLeft)
import Data.List.NonEmpty.Extra (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Void (Void)
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
import Utils (Annotation (..), ErrorMessageInfo (..), ErrorType (..), Span (..), tshow)
import Prelude hiding (span)

errorMessages :: Maybe FilePath -> FileContents -> [ErrorMessageInfo Int] -> Text
errorMessages name contents errors =
  T.concat . map formatErrorMessage $ errorBundleMessages bundle
  where
    bundle = mkErrorBundle name' contents errors
    name' = mkFilePathName name

mkFilePathName :: Maybe FilePath -> String
mkFilePathName = fromMaybe "<input>"

type CompileResult a = Either (ErrorMessageInfo Int) a

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

extractErorrType :: CompileResult a -> Either ErrorType a
extractErorrType = mapLeft (\(ErrorMessageInfo {errorType}) -> errorType)

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
errorBundleMessages (ErrorBundle {posState, errors}) = go posState errors
  where
    go ::
      PosState Text ->
      [ErrorMessageInfo Int] ->
      [ErrorMessageInfo OffendingLine]
    go _ [] = []
    go s (e : es) =
      let (errorMessageInfo, s') = attachOffendingLines s e
       in errorMessageInfo : go s' es

attachOffendingLines :: PosState Text -> ErrorMessageInfo Int -> (ErrorMessageInfo OffendingLine, PosState Text)
attachOffendingLines posState err@(ErrorMessageInfo {annotations}) =
  let (annotations', posState') = go posState annotations
   in (err {annotations = annotations'}, posState')
  where
    go :: PosState Text -> [Annotation Int] -> ([Annotation OffendingLine], PosState Text)
    go posState [] = ([], posState)
    go posState (a : as) =
      let (a', posState') = attachOffendingLine posState a
          (as', posState'') = go posState' as
       in (a' : as', posState'')

attachOffendingLine :: PosState Text -> Annotation Int -> (Annotation OffendingLine, PosState Text)
attachOffendingLine posState ann@(Annotation {span = s@(Span {location = offset})}) =
  let (maybeLine, posState') = reachOffset offset posState
      line = pack $ fromMaybe "" maybeLine
      sourcePos = pstateSourcePos posState'
      offendingLine = OffendingLine {line, sourcePos}
      ann' = ann {span = s {location = offendingLine}}
   in (ann', posState')

spanEnd :: Span Int -> Int
spanEnd (Span {location, length}) = location + length

-- Extends the first span to include the length of the second
addLength :: Span Int -> Span Int -> Span Int
addLength s1 s2 = s1 {length = spanEnd s2 - location s1}

data OffendingLine = OffendingLine
  { line :: Text,
    sourcePos :: SourcePos
  }

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
               Just helpText -> "help: " <> helpText
           )
        <> "\n"

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
                let indentedOthers = map (T.replicate indentation " " <>) others
                    indentation = 4 + line1And3PaddingCount + columnZeroIndexed
                 in T.unlines $ l : indentedOthers
   in T.unlines [line1, line2, line3]

genericErrorInfo :: ErrorType -> ErrorMessageInfo Int
genericErrorInfo errorType =
  ErrorMessageInfo
    { errorType = InvalidRuleDefinition,
      message = "error message not yet implemented for " <> tshow errorType,
      annotations = [],
      help = Nothing
    }

parseErrorMessage :: ParseErrorBundle Text Void -> ErrorMessageInfo Int
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

badEllipsesCountErrorMessage :: Int -> Int -> Span Int -> Span Int -> ErrorMessageInfo Int
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
              annotation = "matched with " <> tshow requiredCount <> " ellipses in pattern"
            },
          Annotation
            { span = constructorVar,
              annotation = "used with " <> tshow actualCount <> " ellipses in constructor"
            }
        ],
      help = Just "variables must be used with the same number of ellipses they were matched with"
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

-- badEllipsesCount :: Int -> Int -> Span -> Span -> ErrorMessageInfo
-- badEllipsesCount requiredCount actualCount patternVarSpan constructorVarSpan =
--   ErrorMessageInfo
--     { errorType = BadEllipsesCount,
--       message = "too few ellipses",
--       annotations =
--         [ Annotation
--             { span = constructorVarSpan,
--               annotation = "used with " <> tshow actualCount <> " ellipses here"
--             },
--           Annotation
--             { span = patternVarSpan,
--               annotation = "used with " <> tshow requiredCount <> " ellipses here"
--             }
--         ],
--       help = Just "variables must be used with the same number of ellipses in the pattern and constructor"
--     }

-- parseError :: ParseErrorBundle Text Void -> ErrorMessageInfo
-- parseError (ParseErrorBundle {bundleErrors, bundlePosState}) =
--   ErrorMessageInfo
--     { errorType = ParsingError,
--       message = "bad syntax",
--       annotations = toList $ go <$> fst (attachSourcePos errorOffset bundleErrors bundlePosState),
--       help = Nothing
--     }
--   where
--     go :: (ParseError Text Void, SourcePos) -> Annotation
--     go (e, SourcePos {sourceName, sourceLine, sourceColumn}) =
--       Annotation
--         { span =
--             Span
--               { source = pack sourceName,
--                 start = Pos {line = unPos sourceLine, column = unPos sourceColumn},
--                 end = Pos {line = unPos sourceLine, column = 1 + unPos sourceColumn}
--               },
--           annotation = pack $ parseErrorTextPretty e
--         }

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

-- checkSpanInvariants :: Span -> ()
-- checkSpanInvariants span =
--   case (spanPosBothValid span, spanEndIsPastStart span) of
--     (True, True) -> ()
--     (False, _) -> error "invariant broken: Span Pos both valid"
--     (_, False) -> error "invariant broken: Span end is past start"

-- -- spanPosBothValid :: Span -> Bool
-- -- spanPosBothValid (Span {start, end}) = posHasNonnegativeFields start && posHasNonnegativeFields end

-- -- spanEndIsPastStart :: Span -> Bool
-- -- spanEndIsPastStart Span {start, end} = line start < line end || column start < column end

-- -- data Pos = Pos
-- --   { line :: Int,
-- --     column :: Int
-- --   }
-- --   deriving (Eq, Show)

-- -- checkPosInvariants :: Pos -> ()
-- -- checkPosInvariants pos =
-- --   if posHasNonnegativeFields pos
-- --     then ()
-- --     else error "invariant broken: Pos has negative fields"

-- -- posHasNonnegativeFields :: Pos -> Bool
-- -- posHasNonnegativeFields (Pos {line, column}) = line >= 0 && column >= 0

-- newtype SrcMap = SrcMap (H.HashMap Filename FileContents)

-- -- lookupPosLineText :: SrcMap -> Filename -> Pos -> Text
-- -- lookupPosLineText (SrcMap srcmap) source pos =
-- --   let contents :: Text
-- --       contents = case srcmap H.!? source of
-- --         Nothing -> error "invalid filename"
-- --         Just c -> c
-- --       lineText :: Text
-- --       lineText = case T.lines contents L.!? (line pos - 1) of
-- --         Nothing -> error ("invalid pos line: " <> show (line pos - 1))
-- --         Just l -> l
-- --    in lineText

-- -- Returns Text resembling the following, an informative message about
-- -- a compiler error:
-- --
-- --   error[E1]: variable not matched in pattern used in constructor
-- --   source:line:col
-- --      |
-- --   10 | (def foo $x)
-- --      |          ^^ the variable
-- --   help: either delete `$x` or also use it in the pattern
-- --
-- --   For further information about this error, try `rw --explain E1`.
-- formatErrorMessage :: SrcMap -> Text -> ErrorMessageInfo -> Text
-- formatErrorMessage srcmap thisCompilerName (ErrorMessageInfo {errorType, message, annotations, help}) =
--   "error["
--     <> errorCodeText
--     <> "]: "
--     <> message
--     <> "\n"
--     <> formatSrcSnippets srcmap annotations
--     <> ( case help of
--            Nothing -> ""
--            Just helpText -> "help: " <> helpText
--        )
--     <> "\n"
--     <> "For further information about this error, try `"
--     <> thisCompilerName
--     <> " --explain "
--     <> errorCodeText
--     <> "`"
--     <> "\n"
--   where
--     errorCodeText = "E" <> T.justifyRight 3 '0' (tshow $ errorCode errorType)

-- formatSrcSnippets :: SrcMap -> [Annotation] -> Text
-- formatSrcSnippets srcmap = T.concat . map (formatSrcSnippet srcmap)

-- -- Produces text which resembles the following, describing
-- -- a portion of the text file that an error message applies to:
-- --
-- --   rules.txt:10:12
-- --      |
-- --   10 | (def mydef $x)
-- --      |            ^^ the variable
-- formatSrcSnippet :: SrcMap -> Annotation -> Text
-- formatSrcSnippet srcmap annotation =
--   formatSrcloc annotation <> "\n" <> formatAnnotationBlock srcmap annotation

-- -- Produces text which labels the input name, row, and column,
-- -- separated by colons. Like this:
-- --
-- --   rules.txt:10:12
-- formatSrcloc :: Annotation -> Text
-- formatSrcloc (Annotation {span}) =
--   T.intercalate
--     ":"
--     [ source span,
--       tshow . line $ start span,
--       tshow . column $ start span
--     ]

-- -- Produces text which describes the location of an error in
-- -- the source code along with a annotation:
-- --
-- --      |
-- --   10 | (def mydef $x)
-- --      |            ^^ the variable
-- formatAnnotationBlock :: SrcMap -> Annotation -> Text
-- formatAnnotationBlock srcmap (Annotation {span = span@(Span {source, start}), annotation}) =
--   T.unlines [line1, line2, line3]
--   where
--     line1 = T.replicate line1And3PaddingCount " " <> "|"
--     line2 = " " <> lineNumberText <> " | " <> lineText
--     line3 =
--       line1
--         <> " "
--         <> T.replicate (column start) " "
--         <> T.replicate numberOfCarrots "^"
--         <> " "
--         <> annotation

--     lineText :: Text
--     lineText = lookupPosLineText srcmap source start

--     lineNumberText :: Text
--     lineNumberText = tshow $ line start

--     line1And3PaddingCount :: Int
--     line1And3PaddingCount = T.length lineNumberText + 2

--     numberOfCarrots :: Int
--     numberOfCarrots = oneLineLengthSpanned span lineText

-- oneLineLengthSpanned :: Span -> Text -> Int
-- oneLineLengthSpanned span@(Span {start, end}) lineText =
--   let result =
--         seq
--           (checkSpanInvariants span)
--           ( if line start == line end
--               then column end - column start
--               else T.length lineText - column start
--           )
--    in if result < 0
--         then error "span was outside of lineText"
--         else result