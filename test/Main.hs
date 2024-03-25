module Main (main) where

import qualified Ast0
import Data.Char (isSpace)
import Data.Functor.Base (ListF (..))
import Data.Functor.Foldable (Base, ListF (..), fold)
import Data.List (intercalate, intersperse)
import qualified GHC.Show as Gen
import Hedgehog (MonadGen, Property, evalEither, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Property (success)
import qualified Hedgehog.Internal.Range as Gen
import qualified Hedgehog.Internal.Range as Range
import Lex (Token (..))
import qualified Lex
import qualified Read
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Hedgehog (testProperty)
import Text.Read.Lex (isSymbolChar)
import qualified Hedgehog.Internal.Gen as Gen
import qualified Debug.Trace as Debug

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testCase "lex0" $
        assertEqual "" [TSymbol "abc"] (Lex.lex "abc"),
      testCase "lex1" $
        assertEqual "" [TLeft, TRight] (Lex.lex "()"),
      testCase "lex2" $
        assertEqual "" [TLeft, TSymbol "a", TRight] (Lex.lex "(a)"),
      testCase "lex3" $
        assertEqual "" [TLeft, TSymbol "a", TSymbol "b", TRight] (Lex.lex "(a b)"),
      testCase "lex4" $
        assertEqual "" [] (Lex.lex ""),
      testProperty "lex5" eqUnlexLex,
      testCase "read0" $
        assertEqual "" (Right $ Ast0.Symbol "hello-world") (Read.read "hello-world"),
      testCase "read1" $
        assertEqual "" (Right $ Ast0.Compound [Ast0.Symbol "x", Ast0.Symbol "y"]) (Read.read "(x y)"),
      testProperty "read2" termIsReadable
    ]

eqUnlexLex :: Property
eqUnlexLex =
  property $ do
    str <- forAll $ Gen.string (Range.linear 0 100) Gen.ascii
    filter (not . isSpace) str === unlex (Lex.lex str)

unlex :: [Lex.Token] -> String
unlex = fold $ \case
  Nil -> []
  Cons Lex.TLeft acc -> '(' : acc
  Cons Lex.TRight acc -> ')' : acc
  Cons (Lex.TSymbol s) acc -> s ++ acc

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
    g <- evalEither $ (Debug.traceShow term (Read.read term))
    success