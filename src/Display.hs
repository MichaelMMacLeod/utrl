{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Display
  ( display0,
    display1,
    displayC0,
  )
where

import qualified Ast0
import qualified Ast1
import qualified AstC0
import qualified AstC2
import qualified AstC2Assign
import Data.Functor.Foldable (ListF (..), cata)
import Data.List (intercalate)
import qualified Op
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

displayC2Expr :: C2Expr.Expr -> String
displayC2Expr = _

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

displayIndexC0 :: AstC0.Index -> String
displayIndexC0 index = "[" ++ intercalate "," (map displayIndexElementC0 index) ++ "]"

displayIndexElementC0 :: AstC0.IndexElement -> String
displayIndexElementC0 (AstC0.ZeroPlus i) = show i
displayIndexElementC0 (AstC0.LenMinus i) = "(len-" ++ show i ++ ")"
displayIndexElementC0 (AstC0.Between zeroPlusC0 lenMinusC0) = show zeroPlusC0 ++ ".." ++ show lenMinusC0

displayVar :: Var -> String
displayVar v = "var #" ++ show v

displayConstant :: Int -> String
displayConstant = show