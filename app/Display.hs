{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Display
  ( display0,
    display1,
    displayC0,
    displayC1,
    displayStmts,
  )
where

import Ast0 qualified
import Ast1 qualified
import AstC0 qualified
import AstC1 qualified
import ConstantExpr (ConstantExpr (Constant, Var))
import Data.Functor.Foldable (cata)
import Data.List (intercalate)
import Expr (Expr (..))
import Op qualified
import Stmt (Stmt (..))
import Var (Var)

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

displayStmts :: Show a => [Stmt a] -> String
displayStmts = unlines . zipWith indent [0 ..] . map displayStmt
  where
    indent :: Int -> String -> String
    indent lineNum str = show lineNum ++ ":\t" ++ str

displayStmt :: Show a => Stmt a -> String
displayStmt (Stmt.Assign lhs rhs) = displayVar lhs ++ " = " ++ displayExpr rhs
displayStmt (Stmt.PushSymbolToDataStack s) = "data_stack.push(" ++ show s ++ ")"
displayStmt (Stmt.PushIndexToIndexStack i) = "index_stack.push(" ++ displayConstantExpr i ++ ")"
displayStmt (Stmt.PopFromIndexStack count) = "index_stack.pop(" ++ show count ++ " " ++ name ++ ")"
  where
    name = case count of
      1 -> "index"
      _ -> "indices"
displayStmt Stmt.PushIndexedTermToDataStack = "data_stack.push(input[index_stack])"
displayStmt (Stmt.BuildCompoundTermFromDataStack term_count) =
  "data_stack.push(new CompoundTerm(data_stack.pop(" 
  ++ displayConstantExpr term_count 
  ++ ")))"
displayStmt (Stmt.Jump label) = "jump to instruction " ++ show label
displayStmt (Stmt.JumpWhenLessThan label when_var le_var) =
  "jump to instruction "
    ++ show label
    ++ " when "
    ++ displayVar when_var
    ++ " < "
    ++ displayVar le_var

displayVar :: Var -> String
displayVar v = "var #" ++ show v

displayConstant :: Int -> String
displayConstant = show

displayExpr :: Expr -> String
displayExpr (Expr.Var v) = displayVar v
displayExpr (Expr.Constant c) = displayConstant c
displayExpr (Expr.BinOp op lhs rhs) = displayVar lhs ++ opstr ++ displayConstantExpr rhs
  where
    opstr = case op of
      Op.Add -> " + "
      Op.Sub -> " - "
displayExpr Expr.Length = "input[index_stack].length()"

displayConstantExpr :: ConstantExpr -> String
displayConstantExpr (ConstantExpr.Var v) = displayVar v
displayConstantExpr (ConstantExpr.Constant c) = displayConstant c