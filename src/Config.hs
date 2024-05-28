module Config (Config (..), runConfig, main) where

import Control.Monad.Extra (when)
import Data.Text.IO qualified as T
import Data.Text.IO qualified as Text
import Display (display0)
import Environment (createEnvironment, dumpEnvironmentStmts)
import Error (errorMessages)
import Interpret (compileAndRun)
import Options.Applicative
  ( Parser,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    progDesc,
    strOption,
    switch,
    (<**>),
  )
import Options.Applicative.Types (ParserInfo)
import Read qualified
import System.Exit (exitFailure)

data Config = Config
  { rules :: !FilePath,
    input :: !FilePath,
    dumpStmts :: Bool
  }

main :: IO ()
main = runConfig =<< execParser opts

opts :: ParserInfo Config
opts =
  info
    (parseConfig <**> helper)
    ( fullDesc
        <> progDesc "Simple text tree rewriting engine language"
    )

parseConfig :: Parser Config
parseConfig =
  Config
    <$> strOption
      ( long "rules"
          <> help "Rules file"
      )
    <*> strOption
      ( long "input"
          <> help "Program input"
      )
    <*> switch
      ( long "dump-stmts"
          <> help "output assembly statements for debugging"
      )

runConfig :: Config -> IO ()
runConfig c = do
  rules <- Text.readFile c.rules
  ruleAsts <- case Read.read (Just c.rules) rules of
    Left errors -> do
      T.putStr $ errorMessages (Just c.rules) rules errors
      exitFailure
    Right asts -> pure asts
  input <- Text.readFile c.input
  inputAsts <- case Read.read (Just c.input) input of
    Left errors -> do
      T.putStr $ errorMessages (Just c.input) input errors
      exitFailure
    Right asts -> pure asts
  when (Config.dumpStmts c) $
    do
      let e = createEnvironment ruleAsts
      case e of
        Left _ -> pure ()
        Right t -> putStrLn $ dumpEnvironmentStmts t
  case compileAndRun ruleAsts inputAsts of
    Left e -> T.putStr $ errorMessages (Just (Config.rules c)) rules e
    Right output -> do
      putStrLn $ unlines $ map display0 output