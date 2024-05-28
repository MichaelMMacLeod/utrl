{-# LANGUAGE ApplicativeDo #-}

module Config
  ( Config (..),
    run,
    main,
    runConfigAndPrintOutput,
    runConfig,
    readFileUtf8,
  )
where

import Ast0 qualified
import Data.ByteString (ByteString, hPut)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as T
import Display (display0Text)
import Error (CompileResult, errorMessages)
import GHC.IO.Encoding (utf8)
import GHC.IO.Handle (hSetEncoding)
import GHC.IO.Handle.FD (stderr, stdout, withFile)
import GHC.IO.IOMode (IOMode (ReadMode))
import Interpret (compileAndRun, compileWithoutRunning)
import Options.Applicative
  ( Parser,
    auto,
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
    value,
    (<**>),
  )
import Options.Applicative.Builder (option)
import Options.Applicative.Types (ParserInfo)
import Read qualified
import ReadTypes (SrcLocked)

data Config = Config
  { rules :: FilePath,
    input :: Maybe FilePath
  }

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
  rules <-
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
  pure Config {rules, input}

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 filePath = withFile filePath ReadMode $ \h -> do
  hSetEncoding h utf8
  T.hGetContents h

readAsts :: FilePath -> Text -> Either ByteString [SrcLocked Ast0.Ast]
readAsts path contents = encodeErrors path contents (Read.read (Just path) contents)

encodeErrors :: FilePath -> Text -> CompileResult a -> Either ByteString a
encodeErrors path contents result = case result of
  Right a -> pure a
  Left errors ->
    Left . encodeUtf8 $ errorMessages (Just path) contents errors

run :: (FilePath, Text) -> Maybe (FilePath, Text) -> Either ByteString ByteString
run (defsFile, defsText) input = do
  defsAsts <- readAsts defsFile defsText
  case input of
    Nothing -> do
      encodeErrors defsFile defsText (compileWithoutRunning defsAsts)
      pure ""
    Just (inputFile, inputText) -> do
      inputAsts <- readAsts inputFile inputText
      outputAsts <- encodeErrors defsFile defsText (compileAndRun defsAsts inputAsts)
      pure . encodeUtf8 . T.unlines $ map display0Text outputAsts

runConfig :: Config -> IO (Either ByteString ByteString)
runConfig c = do
  defsText <- readFileUtf8 c.rules
  input <- case c.input of
    Nothing -> pure Nothing
    Just inputFile -> do
      inputText <- readFileUtf8 inputFile
      pure $ Just (inputFile, inputText)
  pure $ run (c.rules, defsText) input

runConfigAndPrintOutput :: Config -> IO ()
runConfigAndPrintOutput c = do
  eitherOutput <- runConfig c
  case eitherOutput of
    Left output -> hPut stderr output
    Right output -> hPut stdout output