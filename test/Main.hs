module Main (main) where

import GoldenTests (goldenTests)
import Test.Tasty (defaultMain)

main :: IO ()
main = do
  golds <- goldenTests
  defaultMain golds