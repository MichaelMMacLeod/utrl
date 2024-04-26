module ConstructorTests (tests) where

import qualified AstC0
import Compile (Variables)
import qualified Compile
import qualified Data.HashMap.Strict as H
import Data.Text (Text, unpack)
import qualified Display
import qualified Interpret
import qualified Read
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import Debug.Trace (trace)

tests :: TestTree
tests =
  testGroup
    "constructor tests"
    [ testCase "compile0" $
        constructorTest
          (H.fromList [("xs", [AstC0.ZeroPlus 1, AstC0.Between 1 0, AstC0.Between 1 0])])
          "(list (list xs ..) ..)"
          "(copy (list (list 1 2 3) (list 4 5 6) (list) (list a b c d e f g)))"
          "(list (list 1 2 3) (list 4 5 6) (list) (list a b c d e f g))",
      testCase "compile1" $
        constructorTest
          (H.fromList [("xs", [AstC0.Between 0 0])])
          "(xs ..)"
          "(a b c d e)"
          "(a b c d e)",
      testCase "compile2" $
        constructorTest
          (H.fromList [("x", [AstC0.ZeroPlus 0]), ("y", [AstC0.ZeroPlus 1])])
          "(y x)"
          "(world! Hello)"
          "(Hello world!)",
      testCase "compile3" $
        constructorTest
          (H.fromList [("x", [AstC0.ZeroPlus 0]), ("y", [AstC0.LenMinus 1])])
          "(x y)"
          "(left 0 1 2 3 4 5 right)"
          "(left right)",
      testCase "compile4" $
        constructorTest
          (H.fromList [("x", [AstC0.ZeroPlus 0]), ("y", [AstC0.LenMinus 1])])
          "(x m1 m2 m3 y)"
          "(left 0 1 2 3 4 5 right)"
          "(left m1 m2 m3 right)",
      testCase "compile5" $
        constructorTest
          (H.fromList [("x", [AstC0.ZeroPlus 0]), ("y", [AstC0.LenMinus 1])])
          "(s1 s2 s3 x y)"
          "(left 0 1 2 3 4 5 right)"
          "(s1 s2 s3 left right)",
      testCase "compile6" $
        constructorTest
          (H.fromList [("x", [AstC0.ZeroPlus 0]), ("y", [AstC0.LenMinus 1])])
          "(x y e1 e2 e3)"
          "(left 0 1 2 3 4 5 right)"
          "(left right e1 e2 e3)",
      testCase "compile7" $
        constructorTest
          (H.fromList [("xs", [AstC0.Between 0 0])])
          "(xs .. end)"
          "(a b c)"
          "(a b c end)",
      testCase "compile8" $
        constructorTest
          (H.fromList [("xs", [AstC0.Between 0 0])])
          "(start xs ..)"
          "(a b c)"
          "(start a b c)",
      testCase "compile9" $
        constructorTest
          (H.fromList [("xs", [AstC0.Between 0 0])])
          "(start xs .. end)"
          "(a b c)"
          "(start a b c end)",
      testCase "compile10" $
        constructorTest
          (H.fromList [("xs", [AstC0.Between 0 0, AstC0.Between 0 0])])
          "(xs .. ..)"
          "((a b c) () (1 2 3 4 5))"
          "(a b c 1 2 3 4 5)",
      testCase "compile11" $
        constructorTest
          (H.fromList [("xs", [AstC0.Between 0 0, AstC0.Between 0 0])])
          "(s1 s2 xs .. .. e1 e2 e3)"
          "((a b c) () (1 2 3 4 5))"
          "(s1 s2 a b c 1 2 3 4 5 e1 e2 e3)",
      testCase "compile12" $
        constructorTest
          (H.fromList [("xs", [AstC0.Between 0 0, AstC0.Between 0 0])])
          "((xs ..) ..)"
          "((a b c) () (1 2 3 4 5))"
          "((a b c) () (1 2 3 4 5))",
      testCase "compile13" $
        constructorTest
          (H.fromList [("xs", [AstC0.Between 0 0, AstC0.Between 0 0])])
          "(start (xs ..) ..)"
          "((a b c) () (1 2 3 4 5))"
          "(start (a b c) () (1 2 3 4 5))",
      testCase "compile14" $
        constructorTest
          (H.fromList [("xs", [AstC0.Between 0 0, AstC0.Between 0 0])])
          "(start (xs ..) .. end)"
          "((a b c) () (1 2 3 4 5))"
          "(start (a b c) () (1 2 3 4 5) end)",
      testCase "compile15" $
        constructorTest
          (H.fromList [("xs", [AstC0.Between 0 0, AstC0.Between 0 0])])
          "(s1 (s2 xs .. e2) .. e1)"
          "((a b c) () (1 2 3 4 5))"
          "(s1 (s2 a b c e2) (s2 e2) (s2 1 2 3 4 5 e2) e1)",
      testCase "compile16" $
        constructorTest
          (H.fromList [("xs", [AstC0.Between 0 0, AstC0.Between 0 0])])
          "((x xs ..) ..)"
          "((a b c) () (1 2 3 4 5))"
          "((x a b c) (x) (x 1 2 3 4 5))",
      testCase "compile17" $
        constructorTest
          ( H.fromList
              [ ("n", [AstC0.ZeroPlus 1]),
                ("m", [AstC0.ZeroPlus 2, AstC0.ZeroPlus 1])
              ]
          )
          "(succ (add n m))"
          "(add 3 (succ 2))"
          "(succ (add 3 2))",
      testCase "compile18" $
        constructorTest
          ( H.fromList
              [ ("f", [AstC0.ZeroPlus 1]),
                ("x", [AstC0.ZeroPlus 2, AstC0.Between 1 0])
              ]
          )
          "(list (f x) ..)"
          "(map add1 (list 1 2 3))"
          "(list (add1 1) (add1 2) (add1 3))"
    ]

constructorTest :: Variables -> Text -> Text -> String -> Assertion
constructorTest vars constructor input expectedOutput = do
  let inputAst = head <$> Read.read input
  case inputAst of
    Left e -> assertFailure ("inputAst read error " ++ show e)
    Right inputAst' -> do
      let stmts = do
            constructorAst <- head <$> Read.read constructor
            Compile.compile vars constructorAst
      case stmts of
        Left e -> assertFailure ("constructor compile error " ++ show e)
        Right stmts' ->
          let result = Interpret.interpret inputAst' stmts'
              str = Display.display0 result
           in assertEqual "interpreted output test" expectedOutput str