{-# LANGUAGE ApplicativeDo #-}

module Config
  ( Config (..),
    main,
    runConfig,
    readFileUtf8,
  )
where

import Ast0 qualified
import Cfg (Cfg, compile)
import Compile (errorsToEither)
import CompileTypes
  ( DefinitionStorage,
    Stage (..),
    Storage (..),
    constructor1,
    constructorC0,
    constructorC1,
    constructorC2,
    pattern1,
    patternP0,
  )
import ConfigTypes (Config (..), EmitStages (stage1, stageC2, stageP0), stageC0, stageC1)
import Control.Comonad.Cofree (Cofree)
import Control.Monad (foldM)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Except (ExceptT (ExceptT), except, throwE)
import Data.ByteString (ByteString, hPut)
import Data.Either.Extra (mapLeft)
import Data.Functor.Foldable (Base, Corecursive)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8, strictBuilderToText, textToStrictBuilder)
import Data.Text.IO qualified as T
import Data.Void (Void)
import Display (display0, display0Builder, display1, displayC0, displayC1, displayC2, displayP0)
import Error (CompileResult, emitStageMessage, errorMessages, parseErrorMessage, result, storage)
import ErrorTypes (ErrorMessage, Span)
import GHC.IO.Encoding (utf8)
import GHC.IO.Handle (hSetEncoding)
import GHC.IO.Handle.FD (stderr, stdout, withFile)
import GHC.IO.IOMode (IOMode (ReadMode))
import Interpret (interpret)
import Options.Applicative
  ( Parser,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    optional,
    progDesc,
    strOption,
    switch,
    (<**>),
  )
import Options.Applicative.Types (ParserInfo)
import Read (cmdLineArgEmitStagesParser)
import Read qualified
import ReadTypes (SrcLocked)
import Text.Megaparsec (runParser)
import Text.Megaparsec.Error (ParseErrorBundle)
import Utils (intToText, shortSpanOf, stripSourceLocationInfo)

main :: IO ()
main = execParser opts >>= runConfigAndPrintOutput

opts :: ParserInfo Config
opts =
  info
    (parseConfig <**> helper)
    ( fullDesc
        <> progDesc "Simple text tree rewriting engine language"
    )

parseConfig :: Parser Config
parseConfig = do
  definitions <-
    strOption
      ( long "defs"
          <> help "File containing definitions to compile"
          <> metavar "FILE"
      )
  input <-
    optional $
      strOption
        ( long "input"
            <> help
              ( "File to process using compiled definitions. If no file "
                  <> "is provided, simply check the definitions for errors."
              )
            <> metavar "FILE"
        )
  trace <-
    switch
      ( long "trace"
          <> help "Display intermediate computation steps. Has no effect without '--input'."
      )
  unparsedEmitStagesText <-
    optional $
      strOption
        ( long "emit"
            <> help
              ( "Emit generated code at given stage. Possible values: 1, C0, C1, C2, P0. "
                  <> "Supply multiple stages by separating each by a comma, so to emit "
                  <> "stages C0 and C2, use --emit 'C0,C2'."
              )
            <> metavar "STAGES"
        )
  pure Config {definitions, input, trace, unparsedEmitStagesText}

runConfigAndPrintOutput :: Config -> IO ()
runConfigAndPrintOutput config = do
  outputEither <- runExceptT $ runConfig config
  case outputEither of
    Left errors -> hPut stderr errors
    Right output -> hPut stdout output

runConfig :: Config -> ExceptT ByteString IO ByteString
runConfig config = do
  emitStages <- parseEmitStages $ fromMaybe "" config.unparsedEmitStagesText
  defText <- ExceptT $ Right <$> readFileUtf8 config.definitions
  defAsts <- readAsts config.definitions defText
  let compileResult = compile defAsts
  let x3 :: Either [ErrorMessage] () = errorsToEither $ emittedStagesMessages emitStages compileResult.storage
  let x4 :: Either Text () = mapLeft (errorMessages Nothing defText) x3
  let x5 :: Either ByteString () = mapLeft encodeUtf8 x4
  except x5
  cfg <- encodeErrors config.definitions defText compileResult
  case config.input of
    Nothing ->
      pure ""
    Just input -> do
      inputText <- ExceptT $ Right <$> readFileUtf8 input
      inputAsts <- readAsts input inputText
      ExceptT $ Right <$> runMany config cfg (map stripSourceLocationInfo inputAsts)

runMany :: Config -> Cfg -> [Ast0.Ast] -> IO ByteString
runMany config cfg inputs = encodeUtf8 . T.unlines <$> mapM (runSingle config cfg) inputs

runSingle :: Config -> Cfg -> Ast0.Ast -> IO Text
runSingle config cfg input = display0 <$> foldM (const foldFunc) input numberedReductions
  where
    foldFunc :: (Int, Ast0.Ast) -> IO Ast0.Ast
    foldFunc (i, ast) =
      if config.trace
        then do
          let textBuilder =
                textToStrictBuilder (intToText i)
                  <> textToStrictBuilder ". "
                  <> display0Builder ast
                  <> textToStrictBuilder "\n"
              bytestring = encodeUtf8 $ strictBuilderToText textBuilder
          hPut stdout bytestring
          pure ast
        else pure ast

    numberedReductions :: [(Int, Ast0.Ast)]
    numberedReductions = zip [0 ..] reductions

    reductions :: [Ast0.Ast]
    reductions = interpret cfg input

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 filePath = withFile filePath ReadMode $ \h -> do
  hSetEncoding h utf8
  T.hGetContents h

parseEmitStages :: Text -> ExceptT ByteString IO EmitStages
parseEmitStages unparsedText =
  let parserOutput :: Either (ParseErrorBundle Text Void) EmitStages =
        runParser cmdLineArgEmitStagesParser "value of --emit" unparsedText
   in case parserOutput of
        Right emitStages -> pure emitStages
        Left parseErrorBundle ->
          let errorMessage :: ErrorMessage = parseErrorMessage parseErrorBundle
              errorMessageText :: Text = errorMessages (Just "value of --emit") unparsedText [errorMessage]
           in throwE $ encodeUtf8 errorMessageText

emittedStagesMessages :: EmitStages -> Storage -> [ErrorMessage]
emittedStagesMessages emitStages (Storage dss) = concatMap emittedStageMessages dss
  where
    emittedStageMessages :: DefinitionStorage -> [ErrorMessage]
    emittedStageMessages ds =
      include s1p stage1
        ++ include sP0 stageP0
        ++ include s1c stage1
        ++ include sC0 stageC0
        ++ include sC1 stageC1
        ++ include sC2 stageC2
      where
        include :: Maybe ErrorMessage -> (EmitStages -> Bool) -> [ErrorMessage]
        include maybeMsg shouldDisplay = if shouldDisplay emitStages then maybeToList maybeMsg else []

        unwrapStage :: Stage a -> Maybe a
        unwrapStage = \case
          Pending -> Nothing
          Fail _ -> Nothing
          Success x -> Just x

        mkMsg :: (Corecursive f) => Text -> (f -> Text) -> Cofree (Base f) (Span Int) -> ErrorMessage
        mkMsg s display ast = emitStageMessage (shortSpanOf ast) s (display $ stripSourceLocationInfo ast)

        s1p :: Maybe ErrorMessage
        s1p = mkMsg "1 (pattern part)" display1 <$> unwrapStage ds.pattern1

        s1c :: Maybe ErrorMessage
        s1c = mkMsg "1 (constructor part)" display1 <$> unwrapStage ds.constructor1

        sC0 :: Maybe ErrorMessage
        sC0 = mkMsg "C0" displayC0 <$> unwrapStage ds.constructorC0

        sC1 :: Maybe ErrorMessage
        sC1 = mkMsg "C1" displayC1 <$> unwrapStage ds.constructorC1

        sC2 :: Maybe ErrorMessage
        sC2 = mkMsg "C2" displayC2 <$> unwrapStage ds.constructorC2

        sP0 :: Maybe ErrorMessage
        sP0 = mkMsg "P0" displayP0 <$> unwrapStage ds.patternP0

readAsts :: FilePath -> Text -> ExceptT ByteString IO [SrcLocked Ast0.Ast]
readAsts path contents = encodeErrors path contents (Read.read (Just path) contents)

encodeErrors :: FilePath -> Text -> CompileResult a -> ExceptT ByteString IO a
encodeErrors path contents result = case result.result of
  Right a -> pure a
  Left errors ->
    throwE . encodeUtf8 $ errorMessages (Just path) contents errors