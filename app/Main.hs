module Main (main) where

import Config (Config (Config))
import qualified Config
import qualified Data.Text.IO as Text
import Display (display0)
import Error (CompileError)
import qualified Error as CompileResult
import Interpret (runProgram)
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
    (<**>),
  )
import Options.Applicative.Types (ParserInfo)

main :: IO ()
main = runConfig =<< execParser opts

opts :: ParserInfo Config
opts =
  info
    (parseConfig <**> helper)
    ( fullDesc
        <> progDesc "TTRE (text tree rewriting engine) is a simple term-rewriting programming language"
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

handleCompileError :: CompileError -> IO ()
handleCompileError r = case r of
  CompileResult.ParsecParseError e -> do
    putStrLn "error: incorrect syntax:"
    print e
  CompileResult.TooFewEllipsesInConstructor -> do
    putStrLn "error: too few ellipses in rule constructor"
  CompileResult.TooManyEllipsesInConstructor -> do
    putStrLn "error: too many ellipses in rule constructor"
  CompileResult.VarsNotCapturedUnderSameEllipsisInConstructor -> do
    putStrLn "error: variables not captured under same ellipsis in rule pattern"
    putStrLn "       have been used under same ellipsis in rule constructor"
  CompileResult.EllipsisAppliedToSymbolInConstructor -> do
    putStrLn "error: ellipsis applied to symbol in constructor"
  CompileResult.InvalidRuleDefinition -> do
    putStrLn "error: invalid rule definition"
  CompileResult.MoreThanOneEllipsisInSingleCompoundTermOfPattern -> do
    putStrLn "error: a compound term a rule's pattern contains more than one"
    putStrLn "       ellipsis"
  CompileResult.VariableUsedMoreThanOnceInPattern -> do
    putStrLn "error: variable used more than once in rule pattern"

runConfig :: Config -> IO ()
runConfig c = do
  rules <- Text.readFile $ Config.rules c
  input <- Text.readFile $ Config.input c
  case runProgram rules input of
    Left c -> handleCompileError c
    Right output -> do
      putStrLn $ display0 output