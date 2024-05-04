module Main (main) where

import qualified ConstructorTests
import qualified MiscTests
import qualified ReadTests
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "tests"
    [ ReadTests.tests,
      ConstructorTests.tests,
      MiscTests.tests
    ]