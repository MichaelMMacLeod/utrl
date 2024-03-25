module Main (main) where

import qualified LexTests
import qualified ReadTests
import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified ConstructorTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "tests"
    [ LexTests.tests,
      ReadTests.tests,
      ConstructorTests.tests
    ]