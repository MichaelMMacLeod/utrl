module MiscTests (tests) where

import Ast0 (Ast (..))
import AstC0
  ( Index,
    IndexElement (Between, LenMinus, ZeroPlus),
    getAtC0Index,
  )
import qualified AstP0
import Compile
  ( compile0to1,
    compile0toRuleDefinition,
    compile1toP0,
    ruleDefinitionVariableBindings,
  )
import Data.Either.Extra (fromRight')
import qualified Data.HashMap.Strict as H
import Error (CompileError (..), CompileResult)
import Predicate
  ( IndexedPredicate (IndexedPredicate),
    Predicate (LengthEqualTo, LengthGreaterThanOrEqualTo, SymbolEqualTo),
    applyPredicate,
  )
import qualified Read
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)

compoundToList :: Ast -> [Ast]
compoundToList (Compound xs) = xs
compoundToList _ = error "expected compound in compoundToList"

tests :: TestTree
tests =
  testGroup
    "misc tests"
    [ testCase "getAtC0Index0" $
        assertEqual
          ""
          (compoundToList $ Read.read' "(2 3 4 5 6)")
          ( getAtC0Index
              [Between 2 1]
              (Read.read' "(0 1 2 3 4 5 6 7)")
          ),
      testCase "getAtC0Index1" $
        assertEqual
          ""
          [Read.read' "(2 3 4 5 6)"]
          ( getAtC0Index
              []
              (Read.read' "(2 3 4 5 6)")
          ),
      testCase "getAtC0Index2" $
        assertEqual
          ""
          (compoundToList $ Read.read' "(5 10)")
          ( getAtC0Index
              [Between 1 0, LenMinus 1]
              (Read.read' "(0 (1 2 3 4 5) (6 7 8 9 10))")
          ),
      testCase "getAtC0Index3" $
        assertEqual
          ""
          (compoundToList $ Read.read' "((g h i j k) (l))")
          ( getAtC0Index
              [ZeroPlus 0, Between 1 0, LenMinus 1]
              (Read.read' "(((a (b c) (d e f)) (f (g h i j k)) (k (l))) last)")
          ),
      testCase "applyPredicate0" $
        assertBool
          ""
          ( applyPredicate
              (IndexedPredicate (SymbolEqualTo "a") [Between 1 1])
              (Read.read' "(start a a a a a end)")
          ),
      testCase "applyPredicate1" $
        assertBool
          ""
          ( not $
              applyPredicate
                (IndexedPredicate (SymbolEqualTo "a") [Between 1 1])
                (Read.read' "(start a a middle a a end)")
          ),
      testCase "applyPredicate2" $
        assertBool
          ""
          ( applyPredicate
              (IndexedPredicate (SymbolEqualTo "a") [Between 0 0])
              (Read.read' "()")
          ),
      testCase "applyPredicate3" $
        assertBool
          ""
          ( not $
              applyPredicate
                (IndexedPredicate (SymbolEqualTo "a") [])
                (Read.read' "()")
          ),
      testCase "applyPredicate4" $
        assertBool
          ""
          ( applyPredicate
              (IndexedPredicate (LengthEqualTo 3) [])
              (Read.read' "(1 2 3)")
          ),
      testCase "applyPredicate5" $
        assertBool
          ""
          ( applyPredicate
              (IndexedPredicate (LengthGreaterThanOrEqualTo 1) [Between 0 0])
              (Read.read' "((a) (b c) (d e f) (g h i j))")
          ),
      testCase "applyPredicate5" $
        assertBool
          ""
          ( not $
              applyPredicate
                (IndexedPredicate (LengthGreaterThanOrEqualTo 1) [Between 0 0])
                (Read.read' "((a) (b c) (d e f) () (g h i j))")
          ),
      testCase "compile1ToP00" $
        assertEqual
          ""
          ( AstP0.CompoundWithEllipses
              [AstP0.Symbol "a", AstP0.Symbol "b"]
              (AstP0.Symbol "c")
              [AstP0.Symbol "d", AstP0.Symbol "e"]
          )
          (fromRight' $ compile1toP0 (compile0to1 (Read.read' "(a b c .. d e)"))),
      testCase "compile1ToP01" $
        assertEqual
          ""
          ( AstP0.CompoundWithoutEllipses
              [ AstP0.Symbol "a",
                AstP0.Symbol "b",
                AstP0.Symbol "c",
                AstP0.Symbol "d",
                AstP0.Symbol "e"
              ]
          )
          (fromRight' $ compile1toP0 (compile0to1 (Read.read' "(a b c d e)"))),
      testCase "compile1ToP02" $
        assertEqual
          ""
          (Left MoreThanOneEllipsisInSingleCompoundTermOfPattern)
          (compile1toP0 (compile0to1 (Read.read' "(a .. b c d .. e)"))),
      testCase "ruleDefinitionVariableBindings0" $
        ruleDefinitionVariableBindingsTest
          "(def a a -> 0)"
          (Right [("a", [])]),
      testCase "ruleDefinitionVariableBindings1" $
        ruleDefinitionVariableBindingsTest
          "(def a (a) -> 0)"
          (Right [("a", [ZeroPlus 0])]),
      testCase "ruleDefinitionVariableBindings2" $
        ruleDefinitionVariableBindingsTest
          "(def a b (a b) -> 0)"
          ( Right
              [ ("a", [ZeroPlus 0]),
                ("b", [ZeroPlus 1])
              ]
          ),
      testCase "ruleDefinitionVariableBindings3" $
        ruleDefinitionVariableBindingsTest
          "(def a b (a .. b) -> 0)"
          ( Right
              [ ("a", [Between 0 1]),
                ("b", [LenMinus 1])
              ]
          ),
      testCase "ruleDefinitionVariableBindings4" $
        ruleDefinitionVariableBindingsTest
          "(def a b ((0 a .. b 1 2 3) ..) -> 0)"
          ( Right
              [ ("a", [Between 0 0, Between 1 4]),
                ("b", [Between 0 0, LenMinus 4])
              ]
          ),
      testCase "ruleDefinitionVariableBindings5" $
        ruleDefinitionVariableBindingsTest
          "(def a (a a) -> 0)"
          (Left VariableUsedMoreThanOnceInPattern),
      testCase "ruleDefinitionVariableBindings6" $
        ruleDefinitionVariableBindingsTest
          "(def a (a .. ((((((a)))) ..) ..)) -> 0)"
          (Left VariableUsedMoreThanOnceInPattern)
    ]

ruleDefinitionVariableBindingsTest :: String -> CompileResult [(String, AstC0.Index)] -> Assertion
ruleDefinitionVariableBindingsTest input expected =
  assertEqual
    ""
    (fmap H.fromList expected)
    ( ruleDefinitionVariableBindings $
        fromRight' $
          compile0toRuleDefinition $
            Read.read' input
    )