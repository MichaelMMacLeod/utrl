module ReadTests where

import qualified Ast0
import Data.Text (Text, pack)
import Error (CompileResult)
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
    [ readTest 0 "hello-world" (Right [Ast0.Symbol "hello-world"]),
      readTest 1 "(x y)" (Right [Ast0.Compound [Ast0.Symbol "x", Ast0.Symbol "y"]]),
      readTest 2 "()" (Right [Ast0.Compound []]),
      readTest
        3
        "(     \n\n\r\r \n    )"
        (Right [Ast0.Compound []]),
      readTest
        4
        "(     \n\n\r\r \n    )\n     \r()"
        (Right [Ast0.Compound [], Ast0.Compound []]),
      readTest
        5
        "(a b)(c d)"
        ( Right
            [ Ast0.Compound [Ast0.Symbol "a", Ast0.Symbol "b"],
              Ast0.Compound [Ast0.Symbol "c", Ast0.Symbol "d"]
            ]
        ),
      readTest
        6
        "(add 0 0)\n"
        ( Right
            [Ast0.Compound [Ast0.Symbol "add", Ast0.Symbol "0", Ast0.Symbol "0"]]
        ),
      readTest
        7
        "\nx"
        (Right [Ast0.Symbol "x"]),
      readTest
        8
        "(x )"
        (Right [Ast0.Compound [Ast0.Symbol "x"]]),
      readTest
        9
        "( x)"
        (Right [Ast0.Compound [Ast0.Symbol "x"]]),
      readTest
        10
        "( ()x)"
        (Right [Ast0.Compound [Ast0.Compound [], Ast0.Symbol "x"]]),
      testProperty "readN" termIsReadable
    ]

readTest :: Int -> Text -> CompileResult [Ast0.Ast] -> TestTree
readTest number input expected =
  testCase ("readTest#" ++ show number) $
    assertEqual "" expected (Read.read input)

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
    term <- pack <$> forAll genTerm
    _ <- evalEither $ Read.read term
    success
