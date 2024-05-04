module Interpret2 (interpret2) where

import qualified Ast0
import qualified AstC2
import qualified AstC2Assign
import AstC2Expr (Expr)
import qualified AstC2Expr as Expr
import AstC2ExprVar (Var)
import qualified AstC2Jump
import AstC2Value (Value)
import qualified AstC2Value as Value
import Data.List.Extra ((!?))
import Debug.Trace (trace)
import Interpret2Memory (Memory (Memory))
import qualified Interpret2Memory as Memory
import Utils (iterateMaybe, setNth)
import qualified Display

interpret2 :: AstC2.Ast Int -> Ast0.Ast -> Ast0.Ast
interpret2 prog initialInput =
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
    transition m = trace (Display.displayC2 $ Memory.program m) $ case m of
      Memory
        { Memory.input = _input,
          Memory.program = program,
          Memory.instruction = instruction,
          Memory.dataStack = dataStack,
          Memory.variables = variables
        } -> do
          i <- program !? instruction
          pure $ case i of
            AstC2.Assign (AstC2Assign.Assign lhs rhs) ->
              m
                { Memory.variables = setNth lhs undefined (evalExpr m rhs) variables,
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