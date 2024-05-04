module MiscTests (tests) where

import Ast0 (Ast (..), replace0At)
import AstC0
  ( Index,
    IndexElement (Between, LenMinus, ZeroPlus),
  )
import qualified AstP0
import Compile (compile0to1, compile0toRuleDefinition, compile1toP0, ruleDefinitionPredicates, ruleDefinitionVariableBindings)
import Data.Either.Extra (fromRight')
import Data.Graph.Inductive (Graph (mkGraph))
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import Environment (Environment (Environment), createEnvironment)
import Error (CompileError (..), CompileResult)
import Interpret (runProgram)
import Predicate
  ( IndexedPredicate (IndexedPredicate),
    Predicate (LengthEqualTo, LengthGreaterThanOrEqualTo, SymbolEqualTo),
    applyPredicate,
  )
import qualified Read
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import Utils (getAtC0Index)

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
          ),
      testCase ("replace0At#" ++ show 0) $
        assertEqual
          ""
          (head $ Read.read' "(0 1 2 THREE 4 5)")
          ( replace0At
              (head $ Read.read' "(0 1 2 3 4 5)")
              [3]
              (head $ Read.read' "THREE")
          ),
      replaceAtTest 0 "(0 1 2 3 4 5)" [3] "THREE" "(0 1 2 THREE 4 5)",
      replaceAtTest 1 "(0 (10 11))" [1, 0] "ten" "(0 (ten 11))",
      replaceAtTest 2 "()" [1, 2, 3, 4, 5] "x" "()",
      runProgramTest
        0
        "(def x -> y)"
        "x"
        (Right "y"),
      runProgramTest
        1
        "(def a -> A)\
        \(def b -> B)"
        "a"
        (Right "A"),
      runProgramTest
        2
        "(def a -> A)\
        \(def b -> B)"
        "b"
        (Right "B"),
      runProgramTest
        3
        "(def x y (x y) -> y)"
        "(a (b (c (d (e (f (g h)))))))"
        (Right "h"),
      runProgramTest
        4
        "(def x (x) -> x)"
        "((0))"
        (Right "0"),
      runProgramTest
        5
        "(def x (x) -> x)"
        "(0)"
        (Right "0"),
      runProgramTest
        6
        "(def n (add n 0) -> n)\
        \(def n m (add n (succ m)) -> (succ (add n m)))"
        "(add 0 (succ 0))"
        (Right "(succ 0)"),
      runProgramTest
        7
        "(def n (1 (2 n)) -> n)"
        "(1 (2 3))"
        (Right "3"),
      runProgramTest
        8
        "(def n (1 (2 (3 n))) -> n)"
        "(1 (2 (3 4)))"
        (Right "4"),
      runProgramTest
        9
        "(def (A B) -> C)\
        \(def b -> B)"
        "(A b)"
        (Right "C"),
      runProgramTest
        10
        "(def (A B) -> B)\
        \(def b -> B)"
        "(A (A b))"
        (Right "B"),
      runProgramTest
        11
        "(def n (add n 0) -> n)\
        \(def n m (add n (succ m)) -> (succ (add n m)))"
        "(add 0 (succ (succ 0)))"
        (Right "(succ (succ 0))"),
      runProgramTest
        12
        "(def 1 -> (S 0))\
        \(def 2 -> (S 1))\
        \(def 3 -> (S 2))\
        \(def 4 -> (S 3))\
        \(def 5 -> (S 4))\
        \(def 6 -> (S 5))\
        \(def 7 -> (S 6))\
        \(def 8 -> (S 7))\
        \(def n   (+ n    0)  ->       n)\
        \(def n m (+ n (S m)) -> (+ (S n) m))\
        \(def   (fib       0)   ->    0)\
        \(def   (fib    (S 0))  -> (S 0))\
        \(def n (fib (S (S n))) -> (+ (fib    n)\
        \                             (fib (S n))))\
        \(def     (equal    0     0)  -> true)\
        \(def m   (equal (S m)    0)  -> false)\
        \(def n   (equal    0  (S n)) -> false)\
        \(def m n (equal (S m) (S n)) -> (equal m n))"
        "(equal (fib 6) 8)"
        (Right "true"),
      runProgramTest
        13
        "(def     (equal    0     0)  -> true)\
        \(def m   (equal (S m)    0)  -> false)\
        \(def n   (equal    0  (S n)) -> false)\
        \(def m n (equal (S m) (S n)) -> (equal m n))\
        \(def t e (if true t e) -> t)\
        \(def t e (if false t e) -> e)\
        \(def v b x\
        \  (apply (v -> b) x)\
        \  ->\
        \  (replace (Var v) for x in b))\
        \(def x y z\
        \  (replace (Var x) for y in (Var z))\
        \  ->\
        \  (if (equal x z) y (Var z)))\
        \(def x y f z\
        \  (replace x for y in (apply f z))\
        \  ->\
        \  (apply (replace x for y in f)\
        \         (replace x for y in z)))\
        \"
        "(apply (0 -> (Var 0)) 123)"
        (Right "123"),
      runProgramTest
        14
        "(def x (flatten (list (list x ..) ..)) -> (list x .. ..))\
        \"
        "(flatten (list (list 1 2 3 4 5 6) (list a b c) (list) (list d)))"
        (Right "(list 1 2 3 4 5 6 a b c d)")
    ]

runProgramTest :: Int -> Text -> Text -> CompileResult Text -> TestTree
runProgramTest number rules input expected =
  testCase ("runProgram#" ++ show number) $
    assertEqual
      ""
      (head . Read.read' <$> expected)
      (runProgram rules input)

replaceAtTest :: Int -> Text -> [Int] -> Text -> Text -> TestTree
replaceAtTest number ast index replacement expected =
  testCase ("replace0At#" ++ show number) $
    assertEqual
      ""
      (head $ Read.read' expected)
      ( replace0At
          (head $ Read.read' ast)
          index
          (head $ Read.read' replacement)
      )

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