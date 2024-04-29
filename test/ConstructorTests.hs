module ConstructorTests (tests) where

import qualified AstC0
import qualified AstC2
import qualified AstC2Assign
import AstC2ConstExpr (ConstExpr)
import qualified AstC2ConstExpr as ConstExpr
import AstC2Expr (Expr (ConstExpr))
import qualified AstC2Expr as Expr
import qualified AstC2ExprBinOp as BinOp
import AstC2ExprVar (Var)
import qualified AstC2Jump
import Compile (Variables)
import qualified Compile
import qualified Data.HashMap.Strict as H
import Data.Text (Text, unpack)
import Debug.Trace (trace)
import qualified Display
import GHC.Arr (array, listArray)
import qualified Interpret
import Interpret2 (interpret2)
import qualified Read
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

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
          "(list (add1 1) (add1 2) (add1 3))",
      constructorTest2
        0
        [ assign 0 $ nat 1, -- compound body counter, 1 non-loopy body
          push $ symbol "list",
          assign 1 input, -- loop src
          assignE 1 $ 1 `arrayRef` nat 2,
          assignE 2 $ len 1, -- loop end
          assign 3 input, -- f (not modified in loop)
          assignE 3 $ 3 `arrayRef` nat 1,
          assign 4 $ nat 1, -- loop iteration variable
          jump 15 $ boolE True, -- jump to end of loop
          assignE 5 $ 1 `arrayRef` var 4, -- loop var
          push $ var 3, -- push f
          push $ var 5,
          build $ nat 2, -- build inner (f x), known size
          assignE 4 $ add 4 $ nat 1, -- increment iteration variable
          assignE 0 $ add 0 $ nat 1, -- increment compound body counter
          jump 9 $ lessThan 4 $ var 2, -- jump to start of loop if not done
          build $ var 0 -- build outer (list (f x) ..), unknown size
        ]
        "(map f (list a b c 1 2 3))"
        "(list (f a) (f b) (f c) (f 1) (f 2) (f 3))"
    ]

build :: ConstExpr -> AstC2.Stmt Int
build = AstC2.Build

boolE :: Bool -> Expr
boolE = Expr.ConstExpr . ConstExpr.Bool

jump :: Int -> Expr -> AstC2.Stmt Int
jump label condition = AstC2.Jump $ AstC2Jump.Jump label condition

len :: Var -> Expr
len v = Expr.Length $ ConstExpr.Var v

lessThan :: Var -> ConstExpr -> Expr
lessThan v c = Expr.BinOp $ BinOp.BinOp BinOp.LessThan v c

add :: Var -> ConstExpr -> Expr
add v i = Expr.BinOp $ BinOp.BinOp BinOp.Add v i

arrayRef :: Var -> ConstExpr -> Expr
arrayRef v c = Expr.BinOp $ BinOp.BinOp BinOp.ArrayAccess v c

input :: ConstExpr
input = ConstExpr.Input

nat :: Int -> ConstExpr
nat = ConstExpr.Nat

var :: Var -> ConstExpr
var = ConstExpr.Var

symbol :: String -> ConstExpr
symbol = ConstExpr.Symbol

assign :: Var -> ConstExpr -> AstC2.Stmt Int
assign v c = assignE v $ Expr.ConstExpr c

assignE :: Var -> Expr -> AstC2.Stmt Int
assignE v e = AstC2.Assign $ AstC2Assign.Assign v e

push :: ConstExpr -> AstC2.Stmt Int
push = AstC2.Push

constructorTest2 :: Int -> [AstC2.Stmt Int] -> Text -> Text -> TestTree
constructorTest2 n program input expected =
  let inputAst = head $ Read.read' input
      expectedAst = head $ Read.read' expected
      actualAst = interpret2 program inputAst
   in -- trace (Display.display0 actualAst) $
      testCase ("constructorTest2#" ++ show n) $
        assertEqual
          ""
          expectedAst
          actualAst

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