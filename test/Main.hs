module Main (main) where

import GoldenTests (goldenTests)
import Test.Tasty (defaultMain, localOption, mkTimeout)

main :: IO ()
main = do
  golds <- localOption (mkTimeout 20000000 {- 20 seconds in microseconds-}) <$> goldenTests
  defaultMain golds