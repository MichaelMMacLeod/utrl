module Interpret
  ( runProgram,
    interpret,
  )
where

import Ast0 (index0, replace0At)
import qualified Ast0
import qualified AstC2
import qualified AstC2Assign
import AstC2Expr (Expr)
import qualified AstC2Expr as Expr
import AstC2ExprVar (Var)
import qualified AstC2Jump
import AstC2Value (Value)
import qualified AstC2Value as Value
import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Comonad.Trans.Cofree (CofreeF, ComonadCofree (unwrap))
import qualified Control.Comonad.Trans.Cofree as CCTC
import Data.Foldable (find)
import Data.Functor.Foldable (Corecursive (..), cata)
import Data.Graph.Inductive (Node, context, labNode', lsuc)
import Data.List.Extra ((!?))
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import Environment (Environment (..), createEnvironment)
import Error (CompileResult)
import InterpretMemory (Memory (Memory))
import qualified InterpretMemory as Memory
import Predicate (applyPredicates)
import qualified Read
import Utils (iterateMaybe, setNth)

data Matcher = Matcher
  { _node :: !Node,
    _ast :: !(Cofree Ast0.AstF [Int])
  }

runProgram :: Text -> Text -> CompileResult [Ast0.Ast]
runProgram rules input = do
  environment <- createEnvironment rules
  asts <- Read.read input
  let results = map (uncofree . interpretInEnvironment environment . index0) asts
  pure results

interpretInEnvironment :: Environment -> Cofree Ast0.AstF [Int] -> Cofree Ast0.AstF [Int]
interpretInEnvironment e input =
  let initialMatcher = Matcher (_start e) input
   in _ast $ last $ iterateMaybe (transitionInEnvironment e) initialMatcher

uncofree :: Cofree Ast0.AstF [Int] -> Ast0.Ast
uncofree = cata go
  where
    go :: CofreeF Ast0.AstF [Int] Ast0.Ast -> Ast0.Ast
    go (_ CCTC.:< ast) = embed ast

transitionInEnvironment :: Environment -> Matcher -> Maybe Matcher
transitionInEnvironment environment matcher =
  -- trace (display0 $ uncofree (_ast matcher)) $
  let currentNode = _node matcher
      currentAst = _ast matcher
      graph = _graph environment
      neighbors = lsuc graph currentNode
      maybeNextNode = fst <$> find (\(_, preds) -> applyPredicates preds (uncofree currentAst)) neighbors
   in case maybeNextNode of
        Just nextNode ->
          let constructor = snd $ labNode' $ context graph nextNode
              nextAst = interpret constructor (uncofree currentAst)
              nextNodeNeighbors = lsuc graph nextNode
              newNode =
                if null nextNodeNeighbors
                  then _start environment
                  else nextNode
           in Just $ Matcher newNode (index0 nextAst)
        Nothing -> case unwrap currentAst of
          Ast0.SymbolF _ -> Nothing
          Ast0.CompoundF xs ->
            let xs' :: [Maybe (Matcher, [Int])]
                xs' = flip map xs $ \(index :< x) ->
                  let newMatcher =
                        transitionInEnvironment
                          environment
                          (Matcher (_start environment) (index0 (uncofree (index :< x))))
                   in (\m -> (m, index)) <$> newMatcher
                maybeSubtermMatcher :: Maybe (Matcher, [Int])
                maybeSubtermMatcher = listToMaybe $ catMaybes xs'
             in case maybeSubtermMatcher of
                  Nothing -> Nothing
                  Just (subtermMatcher, subtermIndex) ->
                    let newAst = replace0At (uncofree currentAst) subtermIndex (uncofree (_ast subtermMatcher))
                     in Just $ Matcher (_start environment) (index0 newAst)

interpret :: AstC2.Ast Int -> Ast0.Ast -> Ast0.Ast
interpret prog initialInput =
  head . Memory.dataStack . last $
    iterateMaybe transition initialState
  where
    initialState :: Memory
    initialState =
      Memory
        { Memory.input = initialInput,
          Memory.program = prog,
          Memory.instruction = 0,
          Memory.dataStack = [],
          Memory.variables = []
        }

    transition :: Memory -> Maybe Memory
    transition m = case m of
      Memory
        { Memory.input = _input,
          Memory.program = program,
          Memory.instruction = instruction,
          Memory.dataStack = dataStack,
          Memory.variables = variables
        } ->
          -- trace (Display.displayC2 program) $
          do
            i <- program !? instruction
            pure $ case i of
              AstC2.Assign (AstC2Assign.Assign lhs rhs) ->
                m
                  { Memory.variables =
                      setNth
                        lhs
                        (\var -> error $ "$" ++ show var ++ " is undefined")
                        (evalExpr m rhs)
                        variables,
                    Memory.instruction = instruction + 1
                  }
              AstC2.Push expr ->
                let expr' = evalExpr m expr
                    astExpr = Value.expectAst expr'
                 in m
                      { Memory.dataStack = astExpr : dataStack,
                        Memory.instruction = instruction + 1
                      }
              AstC2.Build termCount ->
                let termCount' = evalExpr m termCount
                    termCountNat = Value.expectNat termCount'
                    newTerm = Ast0.Compound . reverse $ take termCountNat dataStack
                 in m
                      { Memory.dataStack = newTerm : drop termCountNat dataStack,
                        Memory.instruction = instruction + 1
                      }
              AstC2.Jump (AstC2Jump.Jump target condition) ->
                let condition' = evalExpr m condition
                    conditionBool = Value.expectBool condition'
                    nextInstruction =
                      if conditionBool
                        then target
                        else instruction + 1
                 in m {Memory.instruction = nextInstruction}

evalVar :: Memory -> Var -> Value
evalVar m v = Memory.variables m !! v

evalExpr :: Memory -> Expr -> Value
evalExpr m = \case
  Expr.Bool b -> Value.Bool b
  Expr.Var v -> evalVar m v
  Expr.Nat n -> Value.Nat n
  Expr.Symbol s -> Value.Ast $ Ast0.Symbol s
  Expr.Input -> Value.Ast $ Memory.input m
  Expr.BinOp op lhs rhs ->
    let lhs' = evalExpr m lhs
        rhs' = evalExpr m rhs
     in case op of
          Expr.Add ->
            let lhsNat = Value.expectNat lhs'
                rhsNat = Value.expectNat rhs'
             in Value.Nat $ lhsNat + rhsNat
          Expr.Sub ->
            let lhsNat = Value.expectNat lhs'
                rhsNat = Value.expectNat rhs'
             in Value.Nat $ lhsNat - rhsNat
          Expr.ArrayAccess ->
            let lhsAst = Value.expectAst lhs'
                rhsNat = Value.expectNat rhs'
             in case lhsAst of
                  Ast0.Symbol _ -> Value.mkTypeError "Compound" lhs'
                  Ast0.Compound xs -> Value.Ast $ xs !! rhsNat
          Expr.LessThan ->
            let lhsNat = Value.expectNat lhs'
                rhsNat = Value.expectNat rhs'
             in Value.Bool $ lhsNat < rhsNat
  Expr.Length e ->
    let e' = evalExpr m e
        eAst = Value.expectAst e'
     in case eAst of
          Ast0.Symbol _ -> Value.mkTypeError "Compound" e'
          Ast0.Compound xs -> Value.Nat $ length xs