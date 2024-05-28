module GoldenTests (goldenTests) where

import Data.List.Extra (takeWhileEnd)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.Providers (TestName)

goldenTests :: TestTree
goldenTests = _

goldenTest :: FilePath -> TestTree
goldenTest testFilePath = goldenVsString testName testFilePath actualOutput
  where
    -- strip test name from/all/of/this --> this
    testName :: TestName
    testName = takeWhileEnd (/= '/') testFilePath

    actualOutput = runConfig (Config {})