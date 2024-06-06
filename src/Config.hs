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
import ConfigTypes (Config (..))
import Control.Monad (foldM)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Except (ExceptT (ExceptT), throwE)
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
import Read qualified
import ReadTypes (SrcLocked)
import Utils (tshow, uncofree)

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
  pure Config {definitions, input, trace}

runConfigAndPrintOutput :: Config -> IO ()
runConfigAndPrintOutput config = do
  outputEither <- runExceptT $ runConfig config
  case outputEither of
    Left errors -> hPut stderr errors
    Right output -> hPut stdout output

runConfig :: Config -> ExceptT ByteString IO ByteString
runConfig config = do
  defText <- ExceptT $ Right <$> readFileUtf8 config.definitions
  defAsts <- readAsts config.definitions defText
  cfg <- encodeErrors config.definitions defText $ compile defAsts
  case config.input of
    Nothing ->
      pure ""
    Just input -> do
      inputText <- ExceptT $ Right <$> readFileUtf8 input
      inputAsts <- readAsts input inputText
      ExceptT $ Right <$> runMany config cfg (map uncofree inputAsts)

runMany :: Config -> Cfg -> [Ast0.Ast] -> IO ByteString
runMany config cfg inputs = encodeUtf8 . T.unlines <$> mapM (runSingle config cfg) inputs

runSingle :: Config -> Cfg -> Ast0.Ast -> IO Text
runSingle config cfg input = Display.display0Text <$> foldM (const foldFunc) input numberedReductions
  where
    foldFunc :: (Int, Ast0.Ast) -> IO Ast0.Ast
    foldFunc (i, ast) =
      if config.trace
        then do
          hPut stdout . encodeUtf8 $ tshow i <> ". " <> Display.display0Text ast
          hPut stdout "\n"
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

readAsts :: FilePath -> Text -> ExceptT ByteString IO [SrcLocked Ast0.Ast]
readAsts path contents = encodeErrors path contents (Read.read (Just path) contents)

encodeErrors :: FilePath -> Text -> CompileResult a -> ExceptT ByteString IO a
encodeErrors path contents result = case result of
  Right a -> pure a
  Left errors ->
    throwE . encodeUtf8 $ errorMessages (Just path) contents errors