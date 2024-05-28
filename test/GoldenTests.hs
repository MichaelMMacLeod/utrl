module GoldenTests (goldenTests) where

import Config (readFileUtf8, run)
import Control.Monad (guard)
import Data.ByteString (fromStrict)
import Data.ByteString.Lazy (ByteString)
import Data.Either.Extra (fromEither)
import Data.List.Extra (stripSuffix)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import System.Directory.Extra (listDirectory)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsStringDiff)
import Test.Tasty.Runners (TestTree (TestGroup))

goldenTests :: IO TestTree
goldenTests = do
  listing <- map ("./test/programs/" <>) <$> listDirectory "./test/programs"
  goldenTestConfigs <- mkGoldenTestConfigs listing
  let subTestTrees = map goldenTest goldenTestConfigs
  pure $ TestGroup "golden tests" subTestTrees

mkGoldenTestConfigs :: [FilePath] -> IO [GoldenTestConfig]
mkGoldenTestConfigs directoryListing = do
  let baseNameHasInputPairs :: [(FilePath, Bool)]
      baseNameHasInputPairs = do
        defsFileBaseName <- mapMaybe isDefsFile directoryListing
        expectedOutputFileBaseName <- mapMaybe isExpectedOutputFile directoryListing
        guard (defsFileBaseName == expectedOutputFileBaseName)
        let inputFile = defsFileBaseName `elem` mapMaybe isInputFile directoryListing
        pure (defsFileBaseName, inputFile)
  mapM createGoldenTest baseNameHasInputPairs

createGoldenTest :: (FilePath, Bool) -> IO GoldenTestConfig
createGoldenTest (testName, hasInput) = do
  let defsFile = testName <> ".defs"
      expectedOutputFile = testName <> ".expected"
  defsFileContents <- readFileUtf8 defsFile
  input <-
    if hasInput
      then do
        let inputFile = testName <> ".input"
        inputFileContents <- readFileUtf8 inputFile
        pure $ Just (inputFile, inputFileContents)
      else pure Nothing
  pure
    GoldenTestConfig
      { testName,
        defs = (defsFile, defsFileContents),
        expectedOutputFile,
        input
      }

goldenTest :: GoldenTestConfig -> TestTree
goldenTest c = goldenVsStringDiff c.testName diffCmd c.expectedOutputFile actualOutput
  where
    diffCmd ref new = ["diff", "--color=always", "-u", ref, new]

    actualOutput :: IO ByteString
    actualOutput = pure . fromStrict . fromEither $ run c.defs c.input

isDefsFile :: FilePath -> Maybe FilePath
isDefsFile = stripSuffix ".defs"

isExpectedOutputFile :: FilePath -> Maybe FilePath
isExpectedOutputFile = stripSuffix ".expected"

isInputFile :: FilePath -> Maybe FilePath
isInputFile = stripSuffix ".input"

data GoldenTestConfig = GoldenTestConfig
  { testName :: String,
    defs :: (FilePath, Text),
    expectedOutputFile :: FilePath,
    input :: Maybe (FilePath, Text)
  }