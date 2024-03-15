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
import Control.Monad.ST (runST)
import Control.Monad.State.Strict (MonadState (get, put, state), State, gets, modify, runState)
import Data.Functor.Foldable (Base, ListF (..), fold)
import Data.HashMap.Strict qualified as H
import Data.Maybe (fromJust)
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

pushIndexToStackStmts :: AstC1.Index -> State C1ToStmtsState [Stmt]
pushIndexToStackStmts = fold $ \case
  Nil -> return []
  Cons (AstC1.ZeroPlus zp) stmts -> do
    stmts <- stmts
    return $ Stmt.PushIndexToIndexStack (Constant zp) : stmts
  Cons (AstC1.LenMinus lm) stmts -> do
    stmts <- stmts
    var <- gets currentVar
    modify incVar
    let assign = Stmt.Assign {lhs = var, rhs = Expr.Length}
    let sub =
          Stmt.Assign
            { lhs = var,
              rhs =
                Expr.BinOp
                  { Expr.op = Op.Sub,
                    Expr.lhs = var,
                    Expr.rhs = ConstantExpr.Constant lm
                  }
            }
    let push = Stmt.PushIndexToIndexStack $ ConstantExpr.Var var
    return $ [assign, sub, push] ++ stmts

popFromIndexStackStmt :: AstC1.Index -> Stmt
popFromIndexStackStmt =
  Stmt.PopFromIndexStack . length

data C1ToStmtsState = C1ToStmtsState
  { currentVar :: Var,
    currentStmt :: Int,
    iteration_count_var :: Maybe Var
  }

incVar :: C1ToStmtsState -> C1ToStmtsState
incVar (C1ToStmtsState currentVar currentStmt iteration_count_var) = C1ToStmtsState (currentVar + 1) currentStmt iteration_count_var

incStmts :: Int -> C1ToStmtsState -> C1ToStmtsState
incStmts i (C1ToStmtsState currentVar currentStmt iteration_count_var) = C1ToStmtsState currentVar (currentStmt + i) iteration_count_var

setIterationCountVar :: C1ToStmtsState -> C1ToStmtsState
setIterationCountVar (C1ToStmtsState currentVar currentStmt iteration_count_var) = C1ToStmtsState (currentVar + 1) currentStmt (Just currentVar)

initialC1ToStmtsState :: C1ToStmtsState
initialC1ToStmtsState = C1ToStmtsState {currentVar = 0, currentStmt = 0, iteration_count_var = Nothing}

compileC1toStmts :: AstC1.Ast -> [Stmt]
compileC1toStmts = fst . flip runState initialC1ToStmtsState . fold go
  where
    go :: Base AstC1.Ast (State C1ToStmtsState [Stmt]) -> State C1ToStmtsState [Stmt]
    go = \case
      AstC1.SymbolF s -> return [PushSymbolToDataStack s]
      AstC1.CompoundF xs -> do
        modify setIterationCountVar
        xs <- sequence xs
        count <- gets iteration_count_var
        case count of
          Nothing -> error "no iteration counter"
          Just count_var ->
            return $ concat xs ++ [BuildCompoundTermFromDataStack {term_count = ConstantExpr.Var count_var}]
      AstC1.CopyF i -> do
        pushStackStmts <- pushIndexToStackStmts i
        return $ pushStackStmts ++ [Stmt.PushIndexedTermToDataStack, popFromIndexStackStmt i]
      AstC1.LoopF index start end body -> do
        loopVar <- gets currentVar
        modify incVar

        pushStackStmts <- pushIndexToStackStmts index

        lengthVar <- gets currentVar
        modify incVar

        endVar <- gets currentVar
        modify incVar

        count <- gets iteration_count_var
        case count of
          Nothing -> error "no iteration counter"
          Just count_var -> do
            let pushLoopVar = Stmt.PushIndexToIndexStack (ConstantExpr.Var loopVar)
            let popLoopVar = Stmt.PopFromIndexStack 1
            body <- body
            let body' = pushLoopVar : body ++ [popLoopVar]

            initialLabel <- gets currentStmt
            let startLabel = initialLabel + 1 + length pushStackStmts + 5
            let endLabel = startLabel + length body' + 2

            let prologue =
                  [Stmt.Assign {lhs = loopVar, rhs = Expr.Constant start}]
                    ++ pushStackStmts
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
                         Stmt.Jump endLabel
                       ]
            let epilogue =
                  [ Stmt.Assign
                      { lhs = loopVar,
                        rhs =
                          Expr.BinOp
                            { Expr.op = Op.Add,
                              Expr.lhs = loopVar,
                              Expr.rhs = ConstantExpr.Constant 1
                            }
                      },
                    Stmt.Assign
                      {
                        lhs = count_var,
                        rhs =
                          Expr.BinOp
                            { Expr.op = Op.Add,
                              Expr.lhs = count_var,
                              Expr.rhs = ConstantExpr.Constant 1 -- TODO this shouldn't be 1? FIXME
                            }
                      },
                    Stmt.JumpWhenLessThan
                      { label = startLabel,
                        when_var = loopVar,
                        le_var = endVar
                      },
                    popFromIndexStackStmt index
                  ]
            let result = prologue ++ body' ++ epilogue
            modify . incStmts $ length result
            return result
