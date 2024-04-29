module Interpret2 (interpret2) where

import qualified Ast0
import qualified AstC2
import qualified AstC2Assign
import AstC2ConstExpr (ConstExpr)
import qualified AstC2ConstExpr as ConstExpr
import AstC2Expr (Expr)
import qualified AstC2Expr as Expr
import qualified AstC2ExprBinOp as Op
import AstC2ExprVar (Var)
import qualified AstC2Jump
import AstC2Value (Value)
import qualified AstC2Value as Value
import Data.List.Extra ((!?))
import Debug.Trace (trace)
import qualified Display
import Interpret2Memory (Memory (Memory))
import qualified Interpret2Memory as Memory
import Utils (iterateMaybe, setNth)

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
    transition m = case m of
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
              let expr' = evalConstExpr m expr
                  astExpr = Value.expectAst expr'
               in m
                    { Memory.dataStack = astExpr : dataStack,
                      Memory.instruction = instruction + 1
                    }
            AstC2.Build termCount ->
              let termCount' = evalConstExpr m termCount
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

evalConstExpr :: Memory -> ConstExpr -> Value
evalConstExpr m = \case
  ConstExpr.Bool b -> Value.Bool b
  ConstExpr.Var v -> evalVar m v
  ConstExpr.Nat n -> Value.Nat n
  ConstExpr.Symbol s -> Value.Ast $ Ast0.Symbol s
  ConstExpr.Input -> Value.Ast $ Memory.input m

evalExpr :: Memory -> Expr -> Value
evalExpr m = \case
  Expr.ConstExpr ce -> evalConstExpr m ce
  Expr.BinOp op ->
    let lhs' = evalVar m $ Op.lhs op
        rhs' = evalConstExpr m $ Op.rhs op
     in case Op.op op of
          Op.Add ->
            let lhsNat = Value.expectNat lhs'
                rhsNat = Value.expectNat rhs'
             in Value.Nat $ lhsNat + rhsNat
          Op.Sub ->
            let lhsNat = Value.expectNat lhs'
                rhsNat = Value.expectNat rhs'
             in Value.Nat $ lhsNat - rhsNat
          Op.ArrayAccess ->
            let lhsAst = Value.expectAst lhs'
                rhsNat = Value.expectNat rhs'
             in case lhsAst of
                  Ast0.Symbol _ -> Value.mkTypeError "Compound" lhs'
                  Ast0.Compound xs -> Value.Ast $ xs !! rhsNat
          Op.LessThan ->
            let lhsNat = Value.expectNat lhs'
                rhsNat = Value.expectNat rhs'
             in Value.Bool $ lhsNat < rhsNat
  Expr.Length e ->
    let e' = evalConstExpr m e
        eAst = Value.expectAst e'
     in case eAst of
          Ast0.Symbol _ -> Value.mkTypeError "Compound" e'
          Ast0.Compound xs -> Value.Nat $ length xs