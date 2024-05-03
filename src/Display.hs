{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Display
  ( display0,
    display1,
    displayC0,
    displayC1,
    displayStmt,
    displayStmts,
  )
where

import qualified Ast0
import qualified Ast1
import qualified AstC0
import qualified AstC1
import qualified AstC2
import qualified AstC2Assign
import ConstantExpr (ConstantExpr (Constant, Var))
import Data.Functor.Foldable (ListF (..), cata)
import Data.List (intercalate)
import Expr (Expr (..))
import qualified Op
import Stmt (Stmt (..))
import Utils (Cata)
import Var (Var)
import AstC2Expr as C2Expr

displayC2 :: AstC2.Ast Int -> String
displayC2 = addLineNumbers . cata go
  where
    addLineNumbers :: [String] -> String
    addLineNumbers = unlines . zipWith prependLineNumber [0 ..]
      where
        prependLineNumber :: Int -> String -> String
        prependLineNumber number str = show number ++ "\t\t" ++ str

    go :: Cata [AstC2.Stmt Int] [String]
    go = \case
      Nil -> []
      Cons stmt strs -> case stmt of
        AstC2.Assign (AstC2Assign.Assign lhs rhs) ->
          (displayVar lhs ++ " = " ++ displayC2Expr rhs) : strs
        AstC2.Push ce -> _
        AstC2.Build ce -> _
        AstC2.Jump j -> _

-- displayC2Expr :: C2Expr.Expr -> String
-- displayC2Expr 

display0 :: Ast0.Ast -> String
display0 = cata $ \case
  Ast0.SymbolF s -> s
  Ast0.CompoundF xs -> "(" ++ unwords xs ++ ")"

display1 :: Ast1.Ast -> String
display1 =
  display0
    . cata
      ( \case
          Ast1.SymbolF s -> Ast0.Symbol s
          Ast1.CompoundF xs -> Ast0.Compound xs
          Ast1.EllipsesF x -> Ast0.Compound [Ast0.Symbol "Ast1.Ellipses", x]
      )

displayC0 :: AstC0.Ast -> String
displayC0 =
  display1
    . cata
      ( \case
          AstC0.SymbolF s -> Ast1.Symbol s
          AstC0.VariableF i ->
            Ast1.Compound
              [ Ast1.Symbol "AstC0.Variable",
                Ast1.Symbol $ displayIndexC0 i
              ]
          AstC0.CompoundF xs -> Ast1.Compound xs
          AstC0.EllipsesF x -> Ast1.Ellipses x
      )

displayC1 :: AstC1.Ast -> String
displayC1 =
  display0
    . cata
      ( \case
          AstC1.SymbolF s -> Ast0.Symbol s
          AstC1.CompoundF xs -> Ast0.Compound xs
          AstC1.CopyF i ->
            Ast0.Compound
              [Ast0.Symbol "AstC1.Copy", Ast0.Symbol $ displayIndexC1 i]
          AstC1.LoopF index start end body ->
            Ast0.Compound
              [ Ast0.Symbol "AstC1.Loop",
                Ast0.Compound
                  [ Ast0.Symbol ".static_indices=",
                    Ast0.Symbol $ displayIndexC1 index
                  ],
                Ast0.Compound
                  [ Ast0.Symbol ".range=",
                    Ast0.Symbol (show start ++ "..(length-" ++ show end ++ ")")
                  ],
                Ast0.Compound
                  [ Ast0.Symbol ".body=",
                    body
                  ]
              ]
      )

displayIndexC0 :: AstC0.Index -> String
displayIndexC0 index = "[" ++ intercalate "," (map displayIndexElementC0 index) ++ "]"

displayIndexElementC0 :: AstC0.IndexElement -> String
displayIndexElementC0 (AstC0.ZeroPlus i) = show i
displayIndexElementC0 (AstC0.LenMinus i) = "(len-" ++ show i ++ ")"
displayIndexElementC0 (AstC0.Between zeroPlusC0 lenMinusC0) = show zeroPlusC0 ++ ".." ++ show lenMinusC0

displayIndexC1 :: AstC1.Index -> String
displayIndexC1 index = "[" ++ intercalate "," (map displayIndexElementC1 index) ++ "]"

displayIndexElementC1 :: AstC1.IndexElement -> String
displayIndexElementC1 (AstC1.ZeroPlus i) = show i
displayIndexElementC1 (AstC1.LenMinus i) = "(len-" ++ show i ++ ")"

displayStmts :: (Show a) => [Stmt a] -> String
displayStmts = unlines . zipWith indent [0 ..] . map displayStmt
  where
    indent :: Int -> String -> String
    indent lineNum str = show lineNum ++ ":\t" ++ str

displayStmt :: (Show a) => Stmt a -> String
displayStmt (Stmt.Assign l r) = displayVar l ++ " = " ++ displayExpr r
displayStmt (Stmt.PushSymbolToDataStack s) = "data_stack.push(" ++ show s ++ ")"
displayStmt (Stmt.PushIndexToIndexStack i) = "index_stack.push(" ++ displayConstantExpr i ++ ")"
displayStmt (Stmt.PopFromIndexStack count) = "index_stack.pop(" ++ show count ++ " " ++ name ++ ")"
  where
    name = case count of
      1 -> "index"
      _ -> "indices"
displayStmt Stmt.PushIndexedTermToDataStack = "data_stack.push(input[index_stack])"
displayStmt (Stmt.BuildCompoundTermFromDataStack c) =
  "data_stack.push(new CompoundTerm(data_stack.pop("
    ++ displayConstantExpr c
    ++ ")))"
displayStmt (Stmt.Jump l) = "jump to instruction " ++ show l
displayStmt (Stmt.JumpWhenLessThan l w le) =
  "jump to instruction "
    ++ show l
    ++ " when "
    ++ displayVar w
    ++ " < "
    ++ displayVar le

displayVar :: Var -> String
displayVar v = "var #" ++ show v

displayConstant :: Int -> String
displayConstant = show

displayExpr :: Expr -> String
displayExpr (Expr.Var v) = displayVar v
displayExpr (Expr.Constant c) = displayConstant c
displayExpr (Expr.BinOp o l r) = displayVar l ++ opstr ++ displayConstantExpr r
  where
    opstr = case o of
      Op.Add -> " + "
      Op.Sub -> " - "
displayExpr Expr.Length = "input[index_stack].length()"

displayConstantExpr :: ConstantExpr -> String
displayConstantExpr (ConstantExpr.Var v) = displayVar v
displayConstantExpr (ConstantExpr.Constant c) = displayConstant c