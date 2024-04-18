module ReadTests where

import qualified Ast0
import Hedgehog (MonadGen, Property, evalEither, forAll, property)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Property (success)
import qualified Hedgehog.Internal.Range as Range
import qualified Read
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "read tests"
    [ testCase "read0" $
        assertEqual "" (Right $ Ast0.Symbol "hello-world") (Read.read "hello-world"),
      testCase "read1" $
        assertEqual "" (Right $ Ast0.Compound [Ast0.Symbol "x", Ast0.Symbol "y"]) (Read.read "(x y)"),
      testProperty "read2" termIsReadable
    ]

genSymbol :: (MonadGen m) => m String
genSymbol = Gen.string (Range.linear 1 16) Gen.alphaNum

genCompound :: (MonadGen m) => m [String] -> m String
genCompound xs = do
  strings <- xs
  return $ "(" ++ unwords strings ++ ")"

genTerm :: (MonadGen m) => m String
genTerm =
  Gen.recursive
    Gen.choice
    [genSymbol]
    [genCompound (Gen.list (Range.linear 0 16) genTerm)]

termIsReadable :: Property
termIsReadable =
  property $ do
    term <- forAll genTerm
    _ <- evalEither $ Read.read term
    success