{-# LANGUAGE LambdaCase #-}

module MiscTests (tests) where

-- import Ast0 (Ast (..))

import Ast0 (Ast (..))
import AstC0 (IndexElement (Between, LenMinus, ZeroPlus), getAtC0Index)
import qualified Read
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

compoundToList :: Ast -> [Ast]
compoundToList (Compound xs) = xs
compoundToList _ = error "expected compound in compoundToList"

tests :: TestTree
tests =
  testGroup
    "lex tests"
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
          )
    ]