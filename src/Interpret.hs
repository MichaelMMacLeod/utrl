module Interpret (interpret, displayMemory) where

import Ast0 (replace0At)
import qualified Ast0
import ConstantExpr (ConstantExpr (..))
import Control.Comonad.Trans.Cofree (CofreeF)
import Data.Foldable (find)
import Data.Functor.Foldable (cata)
import Data.Graph.Inductive (Node, context, labNode', lsuc)
import Data.List (unfoldr)
import Data.Maybe (catMaybes, fromJust, listToMaybe)
import Debug.Trace (trace)
import Display (display0, displayStmt)
import Environment (Environment (..))
import Expr (Expr (..))
import Op (BinOp (..))
import Predicate (applyPredicates)
import Stmt (Stmt (..))
import Var (Var)

data Matcher = Matcher
  { _node :: !Node,
    _ast :: !Ast0.Ast
  }

interpretInEnvironment :: Environment -> Ast0.Ast -> Ast0.Ast
interpretInEnvironment e input =
  let initialMatcher = Matcher (_start e) input
   in _ast $ last $ iterateMaybe (transitionInEnvironment e) initialMatcher

transitionInEnvironment :: Environment -> Matcher -> Maybe Matcher
transitionInEnvironment environment matcher =
  let currentNode = _node matcher
      currentAst = _ast matcher
      graph = _graph environment
      neighbors = lsuc graph currentNode
      maybeNextNode = fst <$> find (\(_, preds) -> applyPredicates preds currentAst) neighbors
   in case maybeNextNode of
        Just nextNode ->
          let constructor = snd $ labNode' $ context graph nextNode
              nextAst = interpret currentAst constructor
           in Just $ Matcher nextNode nextAst
        Nothing -> case currentAst of
          Ast0.Symbol _ -> Nothing
          Ast0.Compound xs ->
            let xs' :: [Maybe (Matcher, [Int])]
                xs' = flip map xs $ \x ->
                  transitionInEnvironment environment (Matcher (_start environment) x)

                maybeSubtermMatcher :: Maybe (Matcher, [Int])
                maybeSubtermMatcher = listToMaybe $ catMaybes xs'
             in case maybeSubtermMatcher of
                  Nothing -> Nothing
                  Just (subtermMatcher, subtermIndex) ->
                    let newAst = replace0At currentAst subtermIndex (_ast subtermMatcher)
                     in Just $ Matcher currentNode newAst

-- f1 :: Environment -> Matcher -> Ast0.Ast -> Maybe (Matcher, [Int])
-- f1 environment matcher ast = case ast of


-- f1 environment matcher = cata go . Ast0.index0
--   where
--     go :: CofreeF Ast0.AstF [Int] (Maybe (Matcher, [Int])) -> Maybe (Matcher, [Int])
--     go = undefined

-- let nextAst = _
-- Just
-- \$ Matcher {_nodes = nextNode, _ast = nextAst}

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