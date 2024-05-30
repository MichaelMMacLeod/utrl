module Main (main) where

import GoldenTests (goldenTests)
import Test.Tasty (defaultMain, localOption, mkTimeout)

main :: IO ()
main = do
  golds <- localOption (mkTimeout 1000000 {- 1 second in microseconds-}) <$> goldenTests
  defaultMain golds