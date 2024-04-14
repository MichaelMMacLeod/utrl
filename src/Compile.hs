{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Compile
  ( compile,
    compile0to1,
    compile1toC0,
    compileC0toC1,
    compileC1toStmts,
    Variables,
  )
where

import Ast0 qualified
import Ast1 qualified
import AstC0 qualified
import AstC1 qualified
import ConstantExpr (ConstantExpr (..))
import Control.Comonad.Cofree (Cofree ((:<)), ComonadCofree (unwrap))
import Control.Monad.State.Strict (State, gets, modify, runState)
import Data.Functor.Foldable (ListF (..), fold, histo)
import Data.HashMap.Strict ((!?))
import Data.HashMap.Strict qualified as H
import Data.Hashable (Hashable)
import Data.List (uncons)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Error (CompileError (..), CompileResult)
import Expr qualified
import GHC.Generics (Generic)
import Op qualified
import Stmt (Stmt (..))
import Var (Var)
import Control.Comonad (Comonad(..))

type Variables = H.HashMap String AstC0.Index

compile :: Variables -> Ast0.Ast -> CompileResult [Stmt Int]
compile vars ast = do
  let ast1 = compile0to1 ast
  let astc0 = compile1toC0 vars ast1
  astc1 <- compileC0toC1 astc0
  let stmtsWithNamedLabels = compileC1toStmts astc1
  let stmtsWithOffsetLabels = compileLabels stmtsWithNamedLabels
  return stmtsWithOffsetLabels

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

allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual (x : xs) = all (== x) xs

combineCompoundTermIndices :: [Maybe AstC0.Index] -> CompileResult (Maybe AstC0.Index)
combineCompoundTermIndices xs =
  let xs' = catMaybes xs
   in if allEqual xs'
        then Right (fst <$> uncons xs')
        else Left Error.VarsNotCapturedUnderSameEllipsisInConstructor

compileC0toC1 :: AstC0.Ast -> CompileResult AstC1.Ast
compileC0toC1 astC0 = do
  (astC1, index) <- fold go astC0
  case index of
    Nothing -> Right astC1
    Just [] -> Right astC1
    Just _ -> Left Error.TooFewEllipsesInConstructor
  where
    go :: AstC0.AstF (CompileResult (AstC1.Ast, Maybe AstC0.Index)) -> CompileResult (AstC1.Ast, Maybe AstC0.Index)
    go (AstC0.SymbolF s) = Right (AstC1.Symbol s, Nothing)
    go (AstC0.CompoundF xs) = do
      xs' <- sequence xs
      index <- combineCompoundTermIndices (map snd xs')
      return (AstC1.Compound $ map fst xs', index)
    go (AstC0.VariableF i) =
      let (c0Part, c1Part) = AstC0.popTrailingC1Index i
       in Right (AstC1.Copy c1Part, Just c0Part)
    go (AstC0.EllipsesF term) = do
      (astC1, indexC0) <- term
      case indexC0 of
        Nothing ->
          case astC1 of
            AstC1.Symbol _ ->
              Left Error.EllipsisAppliedToSymbolInConstructor
            _other -> error "unreachable, possibly incorrect var bindings"
        Just indexC0' ->
          case AstC0.popBetweenTail indexC0' of
            (indexC0'', Just (zeroPlus, lenMinus)) ->
              let (fstC0, sndC1) = AstC0.popTrailingC1Index indexC0''
                  loopC1 =
                    AstC1.Loop
                      { AstC1.index = sndC1,
                        AstC1.start = zeroPlus,
                        AstC1.end = lenMinus,
                        AstC1.body = astC1
                      }
               in Right (loopC1, Just fstC0)
            (_, Nothing) -> Left Error.TooManyEllipsesInConstructor

newUniqueVar :: State C1ToStmtsState Var
newUniqueVar = do
  var <- gets currentVar
  modify incVar
  return var

pushIndexToStackStmts :: AstC1.Index -> State C1ToStmtsState [Stmt a]
pushIndexToStackStmts = fold $ \case
  Nil -> return []
  Cons (AstC1.ZeroPlus zp) stmts -> do
    stmts' <- stmts
    return $ Stmt.PushIndexToIndexStack (Constant zp) : stmts'
  Cons (AstC1.LenMinus lm) stmts -> do
    stmts' <- stmts
    var <- newUniqueVar
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
    return $ [assign, sub, push] ++ stmts'

popFromIndexStackStmt :: AstC1.Index -> Stmt a
popFromIndexStackStmt =
  Stmt.PopFromIndexStack . length

data C1ToStmtsState = C1ToStmtsState
  { currentVar :: !Var,
    iterationCountVar :: !(Maybe Var)
  }

incVar :: C1ToStmtsState -> C1ToStmtsState
incVar state = state {currentVar = currentVar state + 1}


setIterationCountVar :: C1ToStmtsState -> C1ToStmtsState
setIterationCountVar state = state {currentVar = currentVar state + 1, iterationCountVar = Just $ currentVar state}

initialC1ToStmtsState :: C1ToStmtsState
initialC1ToStmtsState = C1ToStmtsState {currentVar = 0, iterationCountVar = Nothing}

data NamedLabel = TopOfLoop !Int | BotOfLoop !Int deriving (Eq, Generic)

instance Hashable NamedLabel

-- data LoopPrologue = LoopPrologue
--   { -- The variable that will be incremented during each iteration of the loop.
--     var :: !Var,
--     -- The initial value assigned to 'var'.
--     start :: !Int,
--     -- Given an input term (for example, '(a b c d e f g)'), represents the number of terms
--     -- on the right that this loop will ignore. For example, if this
--     -- number is '3', then the loop will iterate over 'a b c d' but will end before reaching
--     -- 'e f g'.
--     numTrailingElementsSkipped :: !Int
--   }

-- makeLoopPrologue :: Var -> Int -> Int -> State C1ToStmtsState [Stmt NamedLabel]
-- makeLoopPrologue var start end = do
--   endVar <- newUniqueVar

--   return $
--     [Stmt.Assign {lhs = var, rhs = Expr.Constant end}]
--       ++ pushStackStmts
--       ++ [ Stmt.Assign {lhs = lengthVar, rhs = Expr.Length},
--            Stmt.Assign {lhs = endVar, rhs = Expr.Constant end},
--            Stmt.Assign
--              { lhs = endVar,
--                rhs =
--                  Expr.BinOp
--                    { Expr.op = Op.Sub,
--                      Expr.lhs = lengthVar,
--                      Expr.rhs = ConstantExpr.Var endVar
--                    }
--              },
--            Stmt.Jump $ TopOfLoop loopLabel
--          ]

compileC1toStmts :: AstC1.Ast -> [Stmt NamedLabel]
compileC1toStmts = fst . flip runState initialC1ToStmtsState . histo go
  where
    shouldIncrementIterationCount :: Cofree AstC1.AstF (State C1ToStmtsState [Stmt NamedLabel]) -> Bool
    shouldIncrementIterationCount (_ :< (AstC1.LoopF {})) = False
    shouldIncrementIterationCount _ = True

    isLoopF :: AstC1.AstF a -> Bool
    isLoopF (AstC1.LoopF {}) = True
    isLoopF _ = False

    go :: AstC1.AstF (Cofree AstC1.AstF (State C1ToStmtsState [Stmt NamedLabel])) -> State C1ToStmtsState [Stmt NamedLabel]
    go = \case
      AstC1.SymbolF s -> return [PushSymbolToDataStack s]
      AstC1.CompoundF xs -> do
        modify setIterationCountVar
        let g = map extract xs :: [State C1ToStmtsState [Stmt NamedLabel]]
            orig = map unwrap xs
            numLoopyBodies = length $ filter isLoopF orig
            numNonLoopyBodies = length orig - numLoopyBodies
        xs' <- sequence g
        count <- gets iterationCountVar
        case count of
          Nothing -> error "no iteration counter"
          Just count_var ->
            return $
              Stmt.Assign {lhs = count_var, rhs = Expr.Constant numNonLoopyBodies}
                : concat xs'
                ++ [BuildCompoundTermFromDataStack {term_count = ConstantExpr.Var count_var}]
      AstC1.CopyF i -> do
        pushStackStmts <- pushIndexToStackStmts i
        return $ pushStackStmts ++ [Stmt.PushIndexedTermToDataStack, popFromIndexStackStmt i]
      AstC1.LoopF index start end body -> do
        loopVar <- newUniqueVar
        lengthVar <- newUniqueVar
        endVar <- newUniqueVar
        loopLabel <- newUniqueVar

        pushStackStmts <- pushIndexToStackStmts index

        count <- gets iterationCountVar

        case count of
          Nothing -> error "uncreachable: no iteration counter"
          Just count_var -> do
            let inc_stmt =
                  ( [ Stmt.Assign
                        { lhs = count_var,
                          rhs =
                            Expr.BinOp
                              { Expr.op = Op.Add,
                                Expr.lhs = count_var,
                                Expr.rhs = ConstantExpr.Constant 1
                              }
                        }
                      | shouldIncrementIterationCount body
                    ]
                  ) ::
                    [Stmt NamedLabel]
            let pushLoopVar = Stmt.PushIndexToIndexStack (ConstantExpr.Var loopVar)
            let popLoopVar = Stmt.PopFromIndexStack 1

            body' <- extract body

            let body'' = pushLoopVar : body' ++ [popLoopVar]

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
                         Stmt.Jump $ TopOfLoop loopLabel
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
                      }
                  ]
                    ++ inc_stmt
                    ++ [ Stmt.JumpWhenLessThan
                           { label = BotOfLoop loopLabel,
                             when_var = loopVar,
                             le_var = endVar
                           },
                         popFromIndexStackStmt index
                       ]
            let result = prologue ++ body'' ++ epilogue
            return result

compileLabels :: [Stmt NamedLabel] -> [Stmt Int]
compileLabels xs = go (calculateInstructionNumbers (zip [0 ..] xs)) xs
  where
    calculateInstructionNumbers :: [(Int, Stmt NamedLabel)] -> H.HashMap NamedLabel Int
    calculateInstructionNumbers =
      H.fromList
        . mapMaybe
          ( \case
              (offset, Jump l) -> Just (l, offset)
              (offset, JumpWhenLessThan l _ _) -> Just (l, offset)
              (_, _) -> Nothing
          )
    go :: H.HashMap NamedLabel Int -> [Stmt NamedLabel] -> [Stmt Int]
    go ht = fold $ \case
      Nil -> []
      Cons stmt others -> stmt' : others
        where
          stmt' = fmap namedInstructionNumber stmt

          namedInstructionNumber :: NamedLabel -> Int
          namedInstructionNumber (TopOfLoop l) = fromJust $ ht !? BotOfLoop l
          namedInstructionNumber (BotOfLoop l) = 1 + fromJust (ht !? TopOfLoop l)
