{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Display
  ( displayC2,
    displayExpr,
    displayP0,
    display0,
    displayC1,
    display1,
    display0Builder,
    displayC0,
    displayC0IndexElement,
    displayC0Index,
    displayC1IndexElement,
    displayC1Index,
  )
where

import Ast0 qualified
import Ast1 qualified
import AstC0 qualified
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
import Data.Text (Text, intercalate)
import Data.Text qualified as T
import Data.Text.Encoding (StrictBuilder, strictBuilderToText, textToStrictBuilder)
import Utils (Cata, tshow)

display0 :: Ast0.Ast -> Text
display0 = strictBuilderToText . display0Builder

display0Builder :: Ast0.Ast -> StrictBuilder
display0Builder = cata go
  where
    go :: Cata Ast0.Ast StrictBuilder
    go = \case
      Ast0.SymbolF s -> textToStrictBuilder s
      Ast0.CompoundF xs -> textToStrictBuilder "(" <> unwordsBuilder xs <> textToStrictBuilder ")"

display1 :: Ast1.Ast -> Text
display1 = display0 . cata go
  where
    go :: Cata Ast1.Ast Ast0.Ast
    go = \case
      Ast1.SymbolF s -> Ast0.Symbol s
      Ast1.CompoundF xs -> Ast0.Compound xs
      Ast1.EllipsesF x -> Ast0.Symbol $ "{Ellipses " <> display0 x <> "}"

displayC0Index :: AstC0.Index -> Text
displayC0Index i = "[" <> intercalate "," (map displayC0IndexElement i) <> "]"

displayC0IndexElement :: AstC0.IndexElement -> Text
displayC0IndexElement = \case
  AstC0.ZeroPlus n -> tshow n
  AstC0.LenMinus n -> "len-" <> tshow n
  AstC0.Between (AstC0.AstC0Between zp lm) ->
    tshow zp <> if lm == 0 then "..len" else "..len-" <> tshow lm

displayP0 :: AstP0.Ast -> Text
displayP0 = display0 . cata go
  where
    go :: Cata AstP0.Ast Ast0.Ast
    go = \case
      AstP0.SymbolF s -> Ast0.Symbol s
      AstP0.CompoundWithoutEllipsesF xs -> Ast0.Compound xs
      AstP0.CompoundWithEllipsesF (AstP0CompoundWtihEllipsesF b e a) ->
        Ast0.Symbol $
          "{Before = ["
            <> intercalate "," (map display0 b)
            <> "], Ellipses = "
            <> display0 e
            <> ", after = ["
            <> intercalate "," (map display0 a)
            <> "]}"

displayC0 :: AstC0.Ast -> Text
displayC0 = display1 . cata go
  where
    go :: Cata AstC0.Ast Ast1.Ast
    go = \case
      AstC0.SymbolF s -> Ast1.Symbol s
      AstC0.CompoundF xs -> Ast1.Compound xs
      AstC0.VariableF i n -> Ast1.Symbol $ "{" <> n <> " in input" <> displayC0Index i <> "}"
      AstC0.EllipsesF xs -> Ast1.Ellipses xs

displayC1 :: AstC1.Ast -> Text
displayC1 = display0 . cata go
  where
    go :: Cata AstC1.Ast Ast0.Ast
    go = \case
      AstC1.SymbolF s -> Ast0.Symbol s
      AstC1.CompoundF xs -> Ast0.Compound xs
      AstC1.AssignmentF (var, index, _) ast ->
        Ast0.Symbol $
          "{let $"
            <> tshow var
            <> " = input"
            <> (if null index then "" else displayC1Index index)
            <> "; "
            <> display0 ast
            <> "}"
      AstC1.CopyF var -> Ast0.Symbol $ "$" <> tshow var
      AstC1.LoopF (AstC1LoopF var src start end body) ->
        Ast0.Symbol $
          "{for $"
            <> tshow var
            <> " in $"
            <> tshow src
            <> "["
            <> tshow start
            <> (if end == 0 then "..len" else "..len-" <> tshow end)
            <> "]; "
            <> display0 body
            <> "}"

displayC1Index :: AstC1.Index -> Text
displayC1Index i = "[" <> intercalate "," (map displayC1IndexElement i) <> "]"

displayC1IndexElement :: AstC1.IndexElement -> Text
displayC1IndexElement = \case
  AstC1.ZeroPlus zp -> displayC0IndexElement $ AstC0.ZeroPlus zp
  AstC1.LenMinus lm -> displayC0IndexElement $ AstC0.LenMinus lm

displayC2 :: AstC2.Ast Int -> Text
displayC2 = addLineNumbers . cata go
  where
    addLineNumbers :: [Text] -> Text
    addLineNumbers = T.unlines . zipWith prependLineNumber [0 ..]
      where
        prependLineNumber :: Int -> Text -> Text
        prependLineNumber number str = tshow number <> ".\t  " <> str

    go :: (Show a) => Cata [AstC2.Stmt a] [Text]
    go = \case
      Nil -> []
      Cons stmt strs -> stmt' : strs
        where
          stmt' :: Text
          stmt' = case stmt of
            AstC2.Assign (AstC2Assign.Assign lhs rhs) ->
              "$" <> tshow lhs <> " = " <> displayExpr rhs
            AstC2.Push e ->
              "push " <> displayExpr e
            AstC2.Build e ->
              "build " <> displayExpr e
            AstC2.Jump (AstC2Jump.Jump target condition) ->
              "jump " <> tshow target <> " if " <> displayExpr condition

displayExpr :: Expr -> Text
displayExpr = cata go
  where
    go :: Cata Expr Text
    go = \case
      Expr.BoolF b -> tshow b
      Expr.VarF v -> "$" <> tshow v
      Expr.NatF n -> tshow n
      Expr.SymbolF s -> tshow s
      Expr.InputF -> "input"
      Expr.LengthF l -> l <> ".length"
      Expr.BinOpF op lhs rhs ->
        case op of
          Expr.Add -> lhs <> " + " <> rhs
          Expr.Sub -> lhs <> " - " <> rhs
          Expr.LessThan -> lhs <> " < " <> rhs
          Expr.ArrayAccess -> lhs <> "[" <> rhs <> "]"

unwordsBuilder :: [StrictBuilder] -> StrictBuilder
unwordsBuilder builders = case builders of
  [] -> mempty
  b : builders -> b <> cata go builders
  where
    go :: Cata [StrictBuilder] StrictBuilder
    go = \case
      Nil -> mempty
      Cons b builders -> textToStrictBuilder " " <> b <> builders