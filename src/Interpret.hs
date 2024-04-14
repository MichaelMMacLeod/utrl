{-# LANGUAGE ImportQualifiedPost #-}

module Interpret (interpret) where

import Ast0 qualified
import ConstantExpr (ConstantExpr (..))
import Control.Monad.State.Strict (State, runState)
import Control.Monad.Trans.State.Strict (evalState, get, gets, modify)
import Data.List (unfoldr)
import Data.Maybe (fromJust)
-- import Debug.Trace (trace)
import Display (display0, displayStmt)
import Expr (Expr (..))
import Op (BinOp (..))
import Stmt (Stmt (..))
import Var (Var)

data Memory = Memory
  { input :: Ast0.Ast,
    instructions :: [Stmt Int],
    currentInstruction :: Int,
    dataStack :: [Ast0.Ast],
    indexStack :: [Int],
    variables :: [Int]
  }
  deriving (Show)

displayMemory :: Memory -> String
displayMemory (Memory input instructions currentInstruction dataStack indexStack variables) =
  if currentInstruction < length instructions
    then
      displayStmt (instructions !! currentInstruction)
        ++ "\n"
        ++ "Vars:\t"
        ++ show variables
        ++ "\n"
        ++ "Index:\t"
        ++ show indexStack
        ++ "\n"
        ++ "Data:\t"
        ++ show (map display0 dataStack)
        ++ "\n"
    else "Done!"

setNth :: Int -> Int -> [Int] -> [Int]
setNth i x xs = left ++ [x] ++ right
  where
    left = take i (xs ++ repeat 0)
    right = drop (i + 1) xs

setCurrentInstruction :: Int -> Memory -> Memory
setCurrentInstruction x m = m {currentInstruction = x}

setDataStack :: [Ast0.Ast] -> Memory -> Memory
setDataStack x m = m {dataStack = x}

setIndexStack :: [Int] -> Memory -> Memory
setIndexStack x m = m {indexStack = x}

setVariables :: [Int] -> Memory -> Memory
setVariables x m = m {variables = x}

nextInstruction :: Memory -> Memory
nextInstruction m@(Memory {currentInstruction = c}) = setCurrentInstruction (c + 1) m

iterateMaybe :: (b -> Maybe b) -> b -> [b]
iterateMaybe f = unfoldr (fmap (\s -> (s, s)) . f)

interpret :: Ast0.Ast -> [Stmt Int] -> Ast0.Ast
interpret i stmts = head . dataStack . last $ iterateMaybe transition initialState
  where
    initialState :: Memory
    initialState =
      Memory
        { input = i,
          instructions = stmts,
          currentInstruction = 0,
          dataStack = [],
          indexStack = [],
          variables = []
        }

    transition :: Memory -> Maybe Memory
    transition m@(Memory input instructions currentInstruction dataStack indexStack variables) =
      -- trace (displayMemory m) $
      if currentInstruction == length instructions
        then Nothing
        else Just $ case instructions !! currentInstruction of
          Assign lhs rhs ->
            m
              { variables = setNth lhs (evalExpr m rhs) variables,
                currentInstruction = currentInstruction + 1
              }
          PushSymbolToDataStack s ->
            m
              { dataStack = Ast0.Symbol s : dataStack,
                currentInstruction = currentInstruction + 1
              }
          PushIndexToIndexStack ce ->
            m
              { indexStack = indexStack ++ [evalConstantExpr m ce],
                currentInstruction = currentInstruction + 1
              }
          PopFromIndexStack index_count ->
            m
              { indexStack = take (length indexStack - index_count) indexStack,
                currentInstruction = currentInstruction + 1
              }
          PushIndexedTermToDataStack ->
            m
              { dataStack = fromJust (termAtIndex indexStack input) : dataStack,
                currentInstruction = currentInstruction + 1
              }
          BuildCompoundTermFromDataStack termCount ->
            let termCount' = evalConstantExpr m termCount
                newTerm = Ast0.Compound . reverse $ take termCount' dataStack
             in m
                  { dataStack = newTerm : drop termCount' dataStack,
                    currentInstruction = currentInstruction + 1
                  }
          Jump label -> m {currentInstruction = label}
          JumpWhenLessThan label when_var le_var ->
            let when_var' = evalConstantExpr m (ConstantExpr.Var when_var)
                le_var' = evalConstantExpr m (ConstantExpr.Var le_var)
                nextInstruction =
                  if when_var' < le_var'
                    then label
                    else currentInstruction + 1
             in m {currentInstruction = nextInstruction}

termAtIndex :: [Int] -> Ast0.Ast -> Maybe Ast0.Ast
termAtIndex [] ast = Just ast
termAtIndex (x : xs) (Ast0.Compound cs) =
  if x < length cs
    then termAtIndex xs (cs !! x)
    else Nothing
termAtIndex _ _ = Nothing

-- go n = case stmts !! n of {}

-- tr :: (Show a) => a -> a
-- tr x = trace (show x) x

evalVar :: Memory -> Var -> Int
evalVar (Memory {variables = vars}) v = vars !! v

evalConstantExpr :: Memory -> ConstantExpr -> Int
evalConstantExpr m (ConstantExpr.Var v) = evalVar m v
evalConstantExpr m (ConstantExpr.Constant c) = c

evalExpr :: Memory -> Expr -> Int
evalExpr m (Expr.Var v) = evalVar m v
evalExpr _ (Expr.Constant c) = c
evalExpr m (BinOp op lhs rhs) =
  let rhs' = evalConstantExpr m rhs
      lhs' = evalVar m lhs
   in case op of
        Add -> lhs' + rhs'
        Sub -> lhs' - rhs'
evalExpr (Memory {input = i, indexStack = index}) Length =
  case fromJust $ termAtIndex index i of
    Ast0.Symbol _ -> error "can't take length of symbol"
    Ast0.Compound cs -> length cs