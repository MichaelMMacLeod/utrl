{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Display
  ( displayC2,
    displayExpr,
    displayP0,
    display0,
    display0Builder,
    displayC1,
  )
where

import Ast0 qualified
import AstC1 (AstC1LoopF (..))
import AstC1 qualified
import AstC2 qualified
import AstC2Assign qualified
import AstC2Expr (Expr)
import AstC2Expr qualified as Expr
import AstC2Jump qualified
import AstP0 (AstP0CompoundWtihEllipsesF (AstP0CompoundWtihEllipsesF))
import AstP0 qualified
import Data.Functor.Foldable (ListF (..), cata)
import Data.Text (Text, unpack)
import Data.Text.Encoding (StrictBuilder, strictBuilderToText, textToStrictBuilder)
import Utils (Cata, tshow)

displayP0 :: AstP0.Ast -> Text
displayP0 = display0 . cata go
  where
    go :: Cata AstP0.Ast Ast0.Ast
    go = \case
      AstP0.SymbolF s -> Ast0.Symbol s
      AstP0.CompoundWithoutEllipsesF xs -> Ast0.Compound xs
      AstP0.CompoundWithEllipsesF (AstP0CompoundWtihEllipsesF b e a) ->
        Ast0.Compound $ b ++ [e, Ast0.Symbol ".."] ++ a

displayC1 :: AstC1.Ast -> String
displayC1 = unpack . display0 . cata go
  where
    go :: Cata AstC1.Ast Ast0.Ast
    go = \case
      AstC1.SymbolF s -> Ast0.Symbol s
      AstC1.CompoundF xs -> Ast0.Compound xs
      AstC1.AssignmentF (var, index, location) ast ->
        Ast0.Compound
          [ Ast0.Symbol $ "{$" <> tshow var <> " = " <> tshow index <> " at " <> tshow location <> "}",
            ast
          ]
      AstC1.CopyF var -> Ast0.Symbol $ "{copy $" <> tshow var <> "}"
      AstC1.LoopF (AstC1LoopF var src start end body) ->
        Ast0.Compound
          [ Ast0.Symbol $ "loop{$" <> tshow var <> " in $" <> tshow src <> "[" <> tshow start <> ".." <> tshow end <> "]}",
            body
          ]

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

unwordsBuilder :: [StrictBuilder] -> StrictBuilder
unwordsBuilder builders = case builders of
  [] -> mempty
  b : builders -> b <> cata go builders
  where
    go :: Cata [StrictBuilder] StrictBuilder
    go = \case
      Nil -> mempty
      Cons b builders -> textToStrictBuilder " " <> b <> builders

display0 :: Ast0.Ast -> Text
display0 = strictBuilderToText . display0Builder

display0Builder :: Ast0.Ast -> StrictBuilder
display0Builder = cata go
  where
    go :: Cata Ast0.Ast StrictBuilder
    go = \case
      Ast0.SymbolF s -> textToStrictBuilder s
      Ast0.CompoundF xs -> textToStrictBuilder "(" <> unwordsBuilder xs <> textToStrictBuilder ")"