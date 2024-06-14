{-# OPTIONS_GHC -Wno-name-shadowing #-}

module GoldenTests (goldenTests) where

import Config (readFileUtf8)
import Control.Monad (guard)
import Data.ByteString.Lazy (ByteString)
import Data.List.Extra (stripSuffix)
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import System.Directory.Extra (listDirectory)
import System.Process.Typed (ExitCode, proc, readProcessInterleaved, runProcess, shell)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsStringDiff)
import Test.Tasty.Runners (TestTree (TestGroup))

goldenTests :: IO TestTree
goldenTests =
  do
    listing <- map ("./test/programs/" <>) <$> listDirectory "./test/programs"
    goldenTestConfigs <- mkGoldenTestConfigs listing
    let subTestTrees = map goldenTest goldenTestConfigs
    pure $ TestGroup "golden tests" subTestTrees

mkGoldenTestConfigs :: [FilePath] -> IO [GoldenTestConfig]
mkGoldenTestConfigs directoryListing = do
  let results :: [(FilePath, Bool, Bool)]
      results = do
        defsFileBaseName <- mapMaybe isDefsFile directoryListing
        expectedOutputFileBaseName <- mapMaybe isExpectedOutputFile directoryListing
        guard (defsFileBaseName == expectedOutputFileBaseName)
        let inputFile = defsFileBaseName `elem` mapMaybe isInputFile directoryListing
            argsFile = defsFileBaseName `elem` mapMaybe isArgsFile directoryListing
        pure (defsFileBaseName, inputFile, argsFile)
  mapM createGoldenTest results

createGoldenTest :: (FilePath, Bool, Bool) -> IO GoldenTestConfig
createGoldenTest (testName, hasInput, hasArgs) = do
  let defsFile = testName <> ".defs"
      expectedOutputFile = testName <> ".expected"
      inputFile =
        if hasInput
          then Just $ testName <> ".input"
          else Nothing
      argsFile =
        if hasArgs
          then Just $ testName <> ".args"
          else Nothing
  pure
    GoldenTestConfig
      { testName,
        defsFile = defsFile,
        expectedOutputFile,
        inputFile,
        argsFile
      }

goldenTest :: GoldenTestConfig -> TestTree
goldenTest c = goldenVsStringDiff c.testName diffCmd c.expectedOutputFile mainOutput
  where
    diffCmd ref new = ["diff", "--color=always", "-u", ref, new]

    mainOutput :: IO ByteString
    mainOutput = do
      (_exitCode, output) <- runCmd
      pure output

    runCmd :: IO (ExitCode, ByteString)
    runCmd = do
      args <- args
      readProcessInterleaved (proc "utrl" args)

    args :: IO [String]
    args = do
      argsFileArgs <- argsFileArgs
      pure $ defsArgs <> argsFileArgs <> inputArgs

    argsFileArgs :: IO [String]
    argsFileArgs =
      case c.argsFile of
        Nothing -> pure []
        Just argsFile -> do
          argsFileContents <- readFileUtf8 argsFile
          pure . words $ T.unpack argsFileContents

    defsArgs :: [String]
    defsArgs = ["--defs", c.defsFile]

    inputArgs :: [String]
    inputArgs = case c.inputFile of
      Nothing -> []
      Just inputFile -> ["--input", inputFile]

isDefsFile :: FilePath -> Maybe FilePath
isDefsFile = stripSuffix ".defs"

isExpectedOutputFile :: FilePath -> Maybe FilePath
isExpectedOutputFile = stripSuffix ".expected"

isInputFile :: FilePath -> Maybe FilePath
isInputFile = stripSuffix ".input"

isArgsFile :: FilePath -> Maybe FilePath
isArgsFile = stripSuffix ".args"

data GoldenTestConfig = GoldenTestConfig
  { testName :: String,
    defsFile :: FilePath,
    expectedOutputFile :: FilePath,
    inputFile :: Maybe FilePath,
    argsFile :: Maybe FilePath
  }