{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Display
  ( display0,
    display1,
    displayC0,
    displayC2,
    displayExpr,
    displayP0,
    display0Text,
  )
where

import qualified Ast0
import qualified Ast1
import qualified AstC0
import qualified AstC2
import qualified AstC2Assign
import AstC2Expr (Expr)
import qualified AstC2Expr as Expr
import qualified AstC2Jump
import qualified AstP0
import Data.Functor.Foldable (ListF (..), cata)
import Data.List (intercalate)
import Utils (Cata)
import Data.Text (Text, pack)
import qualified Data.Text as T

displayP0 :: AstP0.Ast -> String
displayP0 = display0 . cata go
  where
    go :: Cata AstP0.Ast Ast0.Ast
    go = \case
      AstP0.SymbolF s -> Ast0.Symbol s
      AstP0.CompoundWithoutEllipsesF xs -> Ast0.Compound xs
      AstP0.CompoundWithEllipsesF b e a -> Ast0.Compound $ b ++ [e, Ast0.Symbol ".."] ++ a

displayC2 :: (Show a) => AstC2.Ast a -> String
displayC2 = addLineNumbers . cata go
  where
    addLineNumbers :: [String] -> String
    addLineNumbers = unlines . zipWith prependLineNumber [0 ..]
      where
        prependLineNumber :: Int -> String -> String
        prependLineNumber number str = show number ++ "\t" ++ str

    go :: (Show a) => Cata [AstC2.Stmt a] [String]
    go = \case
      Nil -> []
      Cons stmt strs -> stmt' : strs
        where
          stmt' :: String
          stmt' = case stmt of
            AstC2.Assign (AstC2Assign.Assign lhs rhs) ->
              "$" ++ show lhs ++ " = " ++ displayExpr rhs
            AstC2.Push e ->
              "push " ++ displayExpr e
            AstC2.Build e ->
              "build " ++ displayExpr e
            AstC2.Jump (AstC2Jump.Jump target condition) ->
              "jump " ++ show target ++ " if " ++ displayExpr condition

displayExpr :: Expr -> String
displayExpr = cata go
  where
    go :: Cata Expr String
    go = \case
      Expr.BoolF b -> show b
      Expr.VarF v -> "$" ++ show v
      Expr.NatF n -> show n
      Expr.SymbolF s -> show s
      Expr.InputF -> "input"
      Expr.LengthF l -> l ++ ".length"
      Expr.BinOpF op lhs rhs ->
        case op of
          Expr.Add -> lhs ++ " + " ++ rhs
          Expr.Sub -> lhs ++ " - " ++ rhs
          Expr.LessThan -> lhs ++ " < " ++ rhs
          Expr.ArrayAccess -> lhs ++ "[" ++ rhs ++ "]"

display0Text :: Ast0.Ast -> Text
display0Text = cata go
  where
    go :: Cata Ast0.Ast Text
    go = \case
      Ast0.SymbolF s -> pack s
      Ast0.CompoundF xs -> "(" <> T.unwords xs <> ")"

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
          AstC0.VariableF i _s ->
            Ast1.Compound
              [ Ast1.Symbol "AstC0.Variable",
                Ast1.Symbol $ displayIndexC0 i
              ]
          AstC0.CompoundF xs -> Ast1.Compound xs
          AstC0.EllipsesF x -> Ast1.Ellipses x
      )

displayIndexC0 :: AstC0.Index -> String
displayIndexC0 index = "[" ++ intercalate "," (map displayIndexElementC0 index) ++ "]"

displayIndexElementC0 :: AstC0.IndexElement -> String
displayIndexElementC0 (AstC0.ZeroPlus i) = show i
displayIndexElementC0 (AstC0.LenMinus i) = "(len-" ++ show i ++ ")"
displayIndexElementC0 (AstC0.Between zeroPlusC0 lenMinusC0) = show zeroPlusC0 ++ ".." ++ show lenMinusC0