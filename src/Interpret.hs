{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Interpret
  ( runProgram,
    interpret,
  )
where

import Ast0 (index0, index0WithBase, replace0At)
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
import Data.Sequence (Seq (..), fromList, singleton)
import Data.Text (Text)
import Debug.Trace (trace)
import qualified Display
import Environment (Environment (..), createEnvironment)
import Error (CompileResult)
import InterpretMemory (Memory (Memory))
import qualified InterpretMemory as Memory
import Predicate (applyPredicates)
import qualified Read
import Utils (Cata, iterateMaybe, setNth)

data Matcher = Matcher
  { _node :: !Node,
    _ast :: !(Cofree Ast0.AstF [Int])
  }

runProgram :: Text -> Text -> CompileResult [Ast0.Ast]
runProgram rules input = do
  environment <- createEnvironment rules
  asts <- Read.read "LICENSE.txt" input
  let results = map (uncofree . interpretInEnvironment environment . index0) asts
  pure results

interpretInEnvironment :: Environment -> Cofree Ast0.AstF [Int] -> Cofree Ast0.AstF [Int]
interpretInEnvironment e input =
  let results = iterateMaybe (transition e) input
   in --  in last results
      last (trace (unlines $ map (Display.display0 . uncofree) results) results)

uncofree :: Cofree Ast0.AstF [Int] -> Ast0.Ast
uncofree = cata go
  where
    go :: CofreeF Ast0.AstF [Int] Ast0.Ast -> Ast0.Ast
    go (_ CCTC.:< ast) = embed ast

transition :: Environment -> Cofree Ast0.AstF [Int] -> Maybe (Cofree Ast0.AstF [Int])
transition environment ast = go $ singleton $ Matcher (_start environment) ast
  where
    go :: Seq Matcher -> Maybe (Cofree Ast0.AstF [Int])
    go matcherQueue =
      -- trace (show $ map ((\x@(i :< _) -> (i, Display.display0 $ uncofree x)) . _ast) matcherQueue) $
      case matcherQueue of
        Empty -> Nothing
        matcher :<| matcherQueue ->
          case transitionInEnvironment environment matcher of
            Left subtermMatchers ->
              go $ matcherQueue <> subtermMatchers
            Right matcher ->
              case _ast matcher of
                index :< replacementAst ->
                  -- trace (show index ++ " " ++ Display.display0 (uncofree ast)) $
                  Just $
                    index0 $
                      replace0At (uncofree ast) index (uncofree (index :< replacementAst))

-- Returns a single matcher holding the result of successfully applying a rule to the
-- ast in the input matcher. Otherwise, if no rule applies to the ast. returns a list
-- of matchers holding subterms of the ast so they may be later tried in a breadth-first
-- search order.
transitionInEnvironment :: Environment -> Matcher -> Either (Seq Matcher) Matcher
transitionInEnvironment environment matcher =
  let currentNode = _node matcher
      currentAst = _ast matcher
      currentIndex = case currentAst of
        index :< _ -> index
      graph = _graph environment
      neighbors = lsuc graph currentNode
      maybeNextNode = fst <$> find (\(_, preds) -> applyPredicates preds (uncofree currentAst)) neighbors
   in case maybeNextNode of
        Just nextNode ->
          let constructor = snd $ labNode' $ context graph nextNode
              nextAst = interpret constructor $ uncofree currentAst
              nextNodeNeighbors = lsuc graph nextNode
              newNode =
                if null nextNodeNeighbors
                  then _start environment
                  else nextNode
           in Right $ Matcher newNode (index0WithBase currentIndex nextAst)
        Nothing -> Left $ fromList $ case unwrap currentAst of
          Ast0.SymbolF _ -> []
          Ast0.CompoundF xs ->
            flip map xs $ \x ->
              Matcher (_start environment) x

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
          -- trace (Display.display0 _input) $
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

evalExpr :: Memory -> Expr -> Value
evalExpr m = cata go
  where
    go :: Cata Expr Value
    go = \case
      Expr.BoolF b -> Value.Bool b
      Expr.VarF v -> evalVar m v
      Expr.NatF n -> Value.Nat n
      Expr.SymbolF s -> Value.Ast $ Ast0.Symbol s
      Expr.InputF -> Value.Ast $ Memory.input m
      Expr.BinOpF op lhs rhs ->
        case op of
          Expr.Add ->
            let lhsNat = Value.expectNat lhs
                rhsNat = Value.expectNat rhs
             in Value.Nat $ lhsNat + rhsNat
          Expr.Sub ->
            let lhsNat = Value.expectNat lhs
                rhsNat = Value.expectNat rhs
             in Value.Nat $ lhsNat - rhsNat
          Expr.ArrayAccess ->
            let lhsAst = Value.expectAst lhs
                rhsNat = Value.expectNat rhs
             in case lhsAst of
                  Ast0.Symbol _ -> Value.mkTypeError "Compound" lhs
                  Ast0.Compound xs -> Value.Ast $ xs !! rhsNat
          Expr.LessThan ->
            let lhsNat = Value.expectNat lhs
                rhsNat = Value.expectNat rhs
             in Value.Bool $ lhsNat < rhsNat
      Expr.LengthF e ->
        case Value.expectAst e of
          Ast0.Symbol _ -> Value.mkTypeError "Compound" e
          Ast0.Compound xs -> Value.Nat $ length xs

evalVar :: Memory -> Var -> Value
evalVar m v = Memory.variables m !! v