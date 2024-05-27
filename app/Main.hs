module Main (main) where

import Config (Config (Config))
import Config qualified
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

-- handleCompileError :: CompileError -> IO ()
-- handleCompileError r = case r of
--   CompileResult.ParsecParseError e -> do
--     putStrLn "error: incorrect syntax:"
--     print e
--   CompileResult.TooFewEllipsesInConstructor -> do
--     putStrLn "error: too few ellipses in rule constructor"
--   CompileResult.TooManyEllipsesInConstructor -> do
--     putStrLn "error: too many ellipses in rule constructor"
--   CompileResult.VarsNotCapturedUnderSameEllipsisInConstructor -> do
--     putStrLn "error: variables not captured under same ellipsis in rule pattern"
--     putStrLn "       have been used under same ellipsis in rule constructor"
--   CompileResult.EllipsisAppliedToSymbolInConstructor -> do
--     putStrLn "error: ellipsis applied to symbol in constructor"
--   CompileResult.InvalidRuleDefinition -> do
--     putStrLn "error: invalid rule definition"
--   CompileResult.MoreThanOneEllipsisInSingleCompoundTermOfPattern -> do
--     putStrLn "error: a compound term a rule's pattern contains more than one"
--     putStrLn "       ellipsis"
--   CompileResult.VariableUsedMoreThanOnceInPattern -> do
--     putStrLn "error: variable used more than once in rule pattern"
--   CompileResult.OverlappingPatterns (o1, o2) -> do
--     putStrLn "error: overlapping patterns are not allowed"
--     putStrLn ""
--     putStrLn $ "  " ++ displayP0 o1
--     putStrLn   ""
--     putStrLn $ "  " ++ displayP0 o2
--     putStrLn   ""
--     putStrLn   "note: these two patterns can match the same term"

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