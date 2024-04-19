module Interpret (interpret, displayMemory) where

import qualified Ast0
import ConstantExpr (ConstantExpr (..))
import Data.List (unfoldr)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Display (display0, displayStmt)
import Expr (Expr (..))
import Op (BinOp (..))
import Stmt (Stmt (..))
import Var (Var)

data Memory = Memory
  { _input :: !Ast0.Ast,
    _instructions :: ![Stmt Int],
    _currentInstruction :: !Int,
    _dataStack :: ![Ast0.Ast],
    _indexStack :: ![Int],
    _variables :: ![Int]
  }
  deriving (Show)

displayMemory :: Memory -> String
displayMemory (Memory _ instructions currentInstruction dataStack indexStack variables) =
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

iterateMaybe :: (b -> Maybe b) -> b -> [b]
iterateMaybe f = unfoldr (fmap (\s -> (s, s)) . f)

interpret :: Ast0.Ast -> [Stmt Int] -> Ast0.Ast
interpret i stmts = head . _dataStack . last $ iterateMaybe transition initialState
  where
    initialState :: Memory
    initialState =
      Memory
        { _input = i,
          _instructions = stmts,
          _currentInstruction = 0,
          _dataStack = [],
          _indexStack = [],
          _variables = []
        }

    transition :: Memory -> Maybe Memory
    transition m@(Memory input instructions currentInstruction dataStack indexStack variables) =
      -- trace (displayMemory m) $
      if currentInstruction == length instructions
        then Nothing
        else Just $ case instructions !! currentInstruction of
          Assign l r ->
            m
              { _variables = setNth l (evalExpr m r) variables,
                _currentInstruction = currentInstruction + 1
              }
          PushSymbolToDataStack s ->
            m
              { _dataStack = Ast0.Symbol s : dataStack,
                _currentInstruction = currentInstruction + 1
              }
          PushIndexToIndexStack ce ->
            m
              { _indexStack = indexStack ++ [evalConstantExpr m ce],
                _currentInstruction = currentInstruction + 1
              }
          PopFromIndexStack c ->
            m
              { _indexStack = take (length indexStack - c) indexStack,
                _currentInstruction = currentInstruction + 1
              }
          PushIndexedTermToDataStack ->
            m
              { _dataStack = fromJust (termAtIndex indexStack input) : dataStack,
                _currentInstruction = currentInstruction + 1
              }
          BuildCompoundTermFromDataStack termCount ->
            let termCount' = evalConstantExpr m termCount
                newTerm = Ast0.Compound . reverse $ take termCount' dataStack
             in if termCount' > length dataStack
                  then error "internal bug"
                  else
                    m
                      { _dataStack = newTerm : drop termCount' dataStack,
                        _currentInstruction = currentInstruction + 1
                      }
          Jump l -> m {_currentInstruction = l}
          JumpWhenLessThan l w le ->
            let when_var' = evalConstantExpr m (ConstantExpr.Var w)
                le_var' = evalConstantExpr m (ConstantExpr.Var le)
                n =
                  if when_var' < le_var'
                    then l
                    else currentInstruction + 1
             in m {_currentInstruction = n}

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
evalVar (Memory {_variables = vars}) v = vars !! v

evalConstantExpr :: Memory -> ConstantExpr -> Int
evalConstantExpr m (ConstantExpr.Var v) = evalVar m v
evalConstantExpr _ (ConstantExpr.Constant c) = c

evalExpr :: Memory -> Expr -> Int
evalExpr m (Expr.Var v) = evalVar m v
evalExpr _ (Expr.Constant c) = c
evalExpr m (BinOp o l r) =
  let rhs' = evalConstantExpr m r
      lhs' = evalVar m l
   in case o of
        Add -> lhs' + rhs'
        Sub -> lhs' - rhs'
evalExpr (Memory {_input = i, _indexStack = index}) Length =
  case fromJust $ termAtIndex index i of
    Ast0.Symbol _ -> error "can't take length of symbol"
    Ast0.Compound cs -> length cs