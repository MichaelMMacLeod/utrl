{-# LANGUAGE OverloadedStrings #-}

module MiscTests (tests) where

import Ast0 (Ast (..))
import AstC0
  ( Index,
    IndexElement (Between, LenMinus, ZeroPlus),
    getAtC0Index,
  )
import qualified AstP0
import Compile (compile0to1, compile0toRuleDefinition, compile1toP0, ruleDefinitionPredicates, ruleDefinitionVariableBindings)
import Data.Either.Extra (fromRight')
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import Environment (Environment (Environment), createEnvironment)
import Error (CompileError (..), CompileResult)
import Predicate
  ( IndexedPredicate (IndexedPredicate),
    Predicate (LengthEqualTo, LengthGreaterThanOrEqualTo, SymbolEqualTo),
    applyPredicate,
  )
import qualified Read
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import Data.Graph.Inductive (Graph(mkGraph))

tests :: TestTree
tests =
  testGroup
    "misc tests"
    [ getAtC0IndexTest 0 "(0 1 2 3 4 5 6 7)" [Between 2 1] "(2 3 4 5 6)",
      getAtC0IndexTest 1 "(0 (1 2 3 4 5) (6 7 8 9 10))" [Between 1 0, LenMinus 1] "(5 10)",
      getAtC0IndexTest
        2
        "(((a (b c) (d e f)) (f (g h i j k)) (k (l))) last)"
        [ZeroPlus 0, Between 1 0, LenMinus 1]
        "((g h i j k) (l))",
      applyPredicateTest 0 "(start a a a a a end)" (SymbolEqualTo "a") [Between 1 1] True,
      applyPredicateTest 1 "(start a a middle a a end)" (SymbolEqualTo "a") [Between 1 1] False,
      applyPredicateTest 2 "()" (SymbolEqualTo "a") [Between 0 0] True,
      applyPredicateTest 3 "()" (SymbolEqualTo "a") [] False,
      applyPredicateTest 4 "(1 2 3)" (LengthEqualTo 3) [] True,
      applyPredicateTest 5 "((a) (b c) (d e f) (g h i j))" (LengthGreaterThanOrEqualTo 1) [Between 0 0] True,
      applyPredicateTest 6 "((a) (b c) (d e f) () (g h i j))" (LengthGreaterThanOrEqualTo 1) [Between 0 0] False,
      compile1ToP0Test
        0
        "(a b c .. d e)"
        ( Right $
            AstP0.CompoundWithEllipses
              [AstP0.Symbol "a", AstP0.Symbol "b"]
              (AstP0.Symbol "c")
              [AstP0.Symbol "d", AstP0.Symbol "e"]
        ),
      compile1ToP0Test
        1
        "(a b c d e)"
        ( Right $
            AstP0.CompoundWithoutEllipses
              [ AstP0.Symbol "a",
                AstP0.Symbol "b",
                AstP0.Symbol "c",
                AstP0.Symbol "d",
                AstP0.Symbol "e"
              ]
        ),
      compile1ToP0Test 2 "(a .. b c d .. e)" (Left MoreThanOneEllipsisInSingleCompoundTermOfPattern),
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
          (Left VariableUsedMoreThanOnceInPattern),
      testCase "ruleDefinitionPredicates0" $
        ruleDefinitionPredicatesTest
          "(def xs (flatten (list (list xs ..) ..)) -> (list xs .. ..))"
          ( Right
              [ IndexedPredicate (LengthEqualTo 2) [],
                IndexedPredicate (SymbolEqualTo "flatten") [ZeroPlus 0],
                IndexedPredicate (LengthGreaterThanOrEqualTo 1) [ZeroPlus 1],
                IndexedPredicate (SymbolEqualTo "list") [ZeroPlus 1, ZeroPlus 0],
                IndexedPredicate (LengthGreaterThanOrEqualTo 1) [ZeroPlus 1, Between 1 0],
                IndexedPredicate (SymbolEqualTo "list") [ZeroPlus 1, Between 1 0, ZeroPlus 0]
              ]
          )
      --     ,
      -- testCase "createEnvironment0" $
      --   assertEqual
      --     ""
      --     ( Right $
      --         Environment
      --           (mkGraph
      --             [
      --               (0, [])
      --             ]
      --             [])
      --           0
      --     )
      --     ( createEnvironment
      --         "(def n m (add n (succ m)) -> (succ (add n m)))\
      --         \(def n (add n 0) -> n)"
      --     )
    ]

compoundToList :: Ast -> [Ast]
compoundToList (Compound xs) = xs
compoundToList _ = error "expected compound in compoundToList"

getAtC0IndexTest :: Int -> Text -> AstC0.Index -> Text -> TestTree
getAtC0IndexTest number input index expected =
  testCase ("getAtC0IndexTest#" ++ show number) $
    assertEqual
      ""
      (compoundToList $ head $ Read.read' expected)
      ( getAtC0Index
          index
          (head $ Read.read' input)
      )

applyPredicateTest :: Int -> Text -> Predicate -> AstC0.Index -> Bool -> TestTree
applyPredicateTest number input predicate index expected =
  testCase ("applyPredicate#" ++ show number) $
    assertBool
      ""
      ( expected
          == applyPredicate
            (IndexedPredicate predicate index)
            (head $ Read.read' input)
      )

compile1ToP0Test :: Int -> Text -> CompileResult AstP0.Ast -> TestTree
compile1ToP0Test number input expected =
  testCase ("compile1ToP0#" ++ show number) $
    assertEqual
      ""
      expected
      (compile1toP0 (compile0to1 $ head $ Read.read' input))

ruleDefinitionVariableBindingsTest :: Text -> CompileResult [(String, AstC0.Index)] -> Assertion
ruleDefinitionVariableBindingsTest input expected =
  assertEqual
    ""
    (fmap H.fromList expected)
    ( ruleDefinitionVariableBindings $
        fromRight' $
          compile0toRuleDefinition $
            head $
              Read.read' input
    )

ruleDefinitionPredicatesTest :: Text -> CompileResult [IndexedPredicate] -> Assertion
ruleDefinitionPredicatesTest input expected =
  assertEqual
    ""
    expected
    ( ruleDefinitionPredicates $
        fromRight' $
          compile0toRuleDefinition $
            head $
              Read.read' input
    )