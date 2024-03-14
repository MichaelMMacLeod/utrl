{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Compile (compile) where

import Ast0 qualified
import Ast1 qualified
import AstC0 qualified
import AstC1 qualified
import ConstantExpr (ConstantExpr (..))
import Data.Functor.Foldable (Base, ListF (..), fold)
import Data.HashMap.Strict qualified as H
import Expr qualified
import Op qualified
import Op qualified as Expr
import Stmt (Stmt (..))
import Type.Reflection qualified as ConstantExpr
import Type.Reflection qualified as Expr
import Var (Var)

type Variables = H.HashMap String AstC0.Index

compile :: Variables -> Ast0.Ast -> [Stmt]
compile vars = compileC1toStmts . compileC0toC1 . compile1toC0 vars . compile0to1

compile0to1 :: Ast0.Ast -> Ast1.Ast
compile0to1 = fold $ \case
  Ast0.SymbolF s -> Ast1.Symbol s
  Ast0.CompoundF xs -> Ast1.Compound $ go xs
    where
      go :: [Ast1.Ast] -> [Ast1.Ast]
      go (y : Ast1.Symbol ".." : ys) = go $ Ast1.Ellipses y : ys
      go (y : ys) = y : go ys
      go [] = []

compile1toC0 :: Variables -> Ast1.Ast -> AstC0.Ast
compile1toC0 vars = fold $ \case
  Ast1.SymbolF s -> case H.lookup s vars of
    Nothing -> AstC0.Symbol s
    Just index -> AstC0.Variable index
  Ast1.CompoundF xs -> AstC0.Compound xs
  Ast1.EllipsesF x -> AstC0.Ellipses x

compileC0toC1 :: AstC0.Ast -> AstC1.Ast
compileC0toC1 = verify . fold go
  where
    verify :: (AstC1.Ast, AstC0.Index) -> AstC1.Ast
    verify (ast, []) = ast
    verify _ = error "Needs more '..'"

    go :: Base AstC0.Ast (AstC1.Ast, AstC0.Index) -> (AstC1.Ast, AstC0.Index)
    go (AstC0.SymbolF s) = (AstC1.Symbol s, [])
    go (AstC0.CompoundF xs) =
      let indexesAllEqual = allEqual $ map snd xs
          allEqual :: [AstC0.Index] -> Bool
          allEqual [] = True
          allEqual (y : ys) = all (== y) ys
          sharedIndex :: [(AstC1.Ast, AstC0.Index)] -> AstC0.Index
          sharedIndex ((_, i) : _) = i
          sharedIndex _ = []
       in if indexesAllEqual
            then (AstC1.Compound $ map fst xs, sharedIndex xs)
            else error "Variables not matched under same '..' used under same '..'"
    go (AstC0.VariableF i) =
      let (c0Part, c1Part) = AstC0.cutC0 i
       in (AstC1.Copy c1Part, c0Part)
    go (AstC0.EllipsesF (astC1, indexC0)) = case AstC0.cutC0Between indexC0 of
      (indexC0', Just (zeroPlus, lenMinus)) ->
        let (fstC0, sndC1) = AstC0.cutC0 indexC0'
            loopC1 = AstC1.Loop {AstC1.index = sndC1, AstC1.start = zeroPlus, AstC1.end = lenMinus, AstC1.body = astC1}
         in (loopC1, fstC0)
      (_, Nothing) -> error "Too many '..'"

pushIndexToStackStmts :: AstC1.Index -> [Stmt]
pushIndexToStackStmts = fold $ \case
  Nil -> []
  Cons (AstC1.ZeroPlus zp) stmts -> Stmt.PushIndexToIndexStack (Constant zp) : stmts
  Cons (AstC1.LenMinus lm) stmts -> [assign, sub, push] ++ stmts
    where
      var = 3 -- FIXME, new var
      assign = Stmt.Assign {lhs = var, rhs = Expr.Length}
      sub =
        Stmt.Assign
          { lhs = var,
            rhs =
              Expr.BinOp
                { Expr.op = Op.Sub,
                  Expr.lhs = var,
                  Expr.rhs = ConstantExpr.Constant lm
                }
          }
      push = Stmt.PushIndexToIndexStack $ ConstantExpr.Var var

popFromIndexStackStmt :: AstC1.Index -> Stmt
popFromIndexStackStmt =
  Stmt.PopFromIndexStack . length

newtype C1ToStmtsState = C1ToStmtsState
  { currentVar :: Var
  }

compileC1toStmts :: AstC1.Ast -> [Stmt]
compileC1toStmts = fold go
  where 
    go :: Base AstC1.Ast [Stmt] -> [Stmt]
    go = \case
      AstC1.SymbolF s -> [PushSymbolToDataStack s]
      AstC1.CompoundF xs -> concat xs ++ [BuildCompoundTermFromDataStack {term_count = length xs}]
      AstC1.CopyF i -> pushIndexToStackStmts i ++ [Stmt.PushIndexedTermToDataStack, popFromIndexStackStmt i]
      AstC1.LoopF index start end body -> prologue ++ body' ++ epilogue
        where
          loopVar = 0 -- FIXME, create new var
          endVar = 1 -- FIXME, create new var
          lengthVar = 2 -- FIXME, create new var
          prologue =
            [Stmt.Assign {lhs = loopVar, rhs = Expr.Constant start}]
              ++ pushIndexToStackStmts index
              ++ [ Stmt.Assign {lhs = lengthVar, rhs = Expr.Length},
                   Stmt.Assign {lhs = endVar, rhs = Expr.Constant end},
                   Stmt.Assign
                     { lhs = endVar,
                       rhs =
                         Expr.BinOp
                           { Expr.op = Op.Sub,
                             Expr.lhs = lengthVar,
                             Expr.rhs = ConstantExpr.Var endVar
                           }
                     },
                   Stmt.Jump 0 -- FIXME, jump to end of loop
                 ]
          body' = pushLoopVar : body ++ [popLoopVar]
          pushLoopVar = Stmt.PushIndexToIndexStack (ConstantExpr.Var loopVar)
          popLoopVar = Stmt.PopFromIndexStack 1
          epilogue =
            [ Stmt.Assign
                { lhs = loopVar,
                  rhs =
                    Expr.BinOp
                      { Expr.op = Op.Add,
                        Expr.lhs = loopVar,
                        Expr.rhs = ConstantExpr.Constant 1
                      }
                },
              Stmt.JumpWhenLessThan
                { label = 0, -- FIXME, jump to top of body'
                  when_var = loopVar,
                  le_var = endVar
                },
              popFromIndexStackStmt index
            ]
