{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Interpret
  ( compileAndRun,
    runConstructor,
    compileWithoutRunning,
  )
where

import Ast0 qualified
import AstC2 qualified
import AstC2Assign qualified
import AstC2Expr (Expr)
import AstC2Expr qualified as Expr
import AstC2ExprVar (Var)
import AstC2Jump qualified
import AstC2Value (Value)
import AstC2Value qualified as Value
import Cfg (Cfg (..), mkCfg)
import CompileTypes (mkStorage)
import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Cofree qualified as C
import Control.Comonad.Trans.Cofree (CofreeF ((:<)), ComonadCofree (unwrap))
import Data.Foldable (find)
import Data.Functor.Foldable (Corecursive (..), cata)
import Data.Graph.Inductive (Node, context, labNode', lsuc)
import Data.List.Extra (snoc, (!?))
import Data.Sequence (Seq (..), fromList, singleton)
import Error (CompileResult)
import InterpretMemory (Memory (Memory))
import InterpretMemory qualified as Memory
import Predicate (applyPredicates)
import ReadTypes (SrcLocked)
import Utils (Cata, iterateMaybe, setNth, uncofree)

data Matcher = Matcher
  { _node :: !Node,
    _ast :: !(Cofree Ast0.AstF [Int])
  }

compileAndRun ::
  [SrcLocked Ast0.Ast] ->
  [SrcLocked Ast0.Ast] ->
  CompileResult [Ast0.Ast]
compileAndRun defAsts inputAsts = do
  cfg <- mkCfg $ mkStorage defAsts
  let results = map (run cfg . uncofree) inputAsts
  pure results

compileWithoutRunning :: [SrcLocked Ast0.Ast] -> CompileResult ()
compileWithoutRunning defAsts = do
  _cfg <- mkCfg $ mkStorage defAsts
  pure ()

run :: Cfg -> Ast0.Ast -> Ast0.Ast
run cfg = uncofree . runIndexed cfg . index0

runIndexed :: Cfg -> Cofree Ast0.AstF [Int] -> Cofree Ast0.AstF [Int]
runIndexed e input =
  let results = iterateMaybe (applyOneDefinitionBFS e) input
   in --  in foldl' (\_ y -> trace (Display.display0 $ uncofree y) y) input results

      last results

-- last (trace (unlines $ map (Display.display0 . uncofree) results) results)

-- | Recursively searches through 'ast' from the top to bottom in a breadth-first-search order,
-- applying and returning the result of the first matching definition from 'cfg'. Returns
-- 'Nothing' if no such definition exists.
applyOneDefinitionBFS :: Cfg -> Cofree Ast0.AstF [Int] -> Maybe (Cofree Ast0.AstF [Int])
applyOneDefinitionBFS cfg ast = go $ singleton $ Matcher cfg.start ast
  where
    go :: Seq Matcher -> Maybe (Cofree Ast0.AstF [Int])
    go matcherQueue =
      -- trace (show $ fmap ((\x@(i :< _) -> (i, Display.display0 $ uncofree x)) . _ast) matcherQueue) $
      case matcherQueue of
        Empty -> Nothing
        matcher :<| matcherQueue ->
          case applyOneDefinition cfg matcher of
            Left subtermMatchers ->
              go $ matcherQueue <> subtermMatchers
            Right matcher ->
              case _ast matcher of
                index C.:< replacementAst ->
                  -- trace (show index ++ " " ++ Display.display0 (uncofree ast)) $
                  Just $
                    index0 $
                      replace0At (uncofree ast) index (uncofree (index C.:< replacementAst))

-- | Returns a single matcher holding the result of successfully applying a definition to the
-- ast in the input matcher. Otherwise, if no definition applies to the ast. returns a list
-- of matchers holding subterms of the ast so they may be later tried in a breadth-first
-- search order.
applyOneDefinition :: Cfg -> Matcher -> Either (Seq Matcher) Matcher
applyOneDefinition cfg matcher =
  let neighbors = lsuc cfg.graph matcher._node
      maybeNextNode = fst <$> find (\(_, preds) -> applyPredicates preds (uncofree matcher._ast)) neighbors
   in case maybeNextNode of
        Just nextNode ->
          let constructor = snd $ labNode' $ context cfg.graph nextNode
              nextAst = runConstructor constructor $ uncofree matcher._ast
              nextNodeNeighbors = lsuc cfg.graph nextNode
              newNode =
                if null nextNodeNeighbors
                  then cfg.start
                  else nextNode
              currentIndex = case matcher._ast of
                index C.:< _ -> index
           in Right $ Matcher newNode (index0WithBase currentIndex nextAst)
        Nothing -> Left $ fromList $ case unwrap matcher._ast of
          Ast0.SymbolF _ -> []
          Ast0.CompoundF xs ->
            flip map xs $ \x ->
              Matcher cfg.start x

runConstructor :: AstC2.Ast Int -> Ast0.Ast -> Ast0.Ast
runConstructor constructor input =
  head . Memory.dataStack . last $
    iterateMaybe transition initialState
  where
    initialState :: Memory
    initialState =
      Memory
        { Memory.input = input,
          Memory.program = constructor,
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

index0 :: Ast0.Ast -> Cofree Ast0.AstF [Int]
index0 ast = cata go ast []
  where
    go :: Ast0.AstF ([Int] -> Cofree Ast0.AstF [Int]) -> [Int] -> Cofree Ast0.AstF [Int]
    go (Ast0.SymbolF s) index = index C.:< Ast0.SymbolF s
    go (Ast0.CompoundF xs) index = index C.:< Ast0.CompoundF (zipWith (. snoc index) xs [0 ..])

index0WithBase :: [Int] -> Ast0.Ast -> Cofree Ast0.AstF [Int]
index0WithBase base ast = cata go ast base
  where
    go :: Ast0.AstF ([Int] -> Cofree Ast0.AstF [Int]) -> [Int] -> Cofree Ast0.AstF [Int]
    go (Ast0.SymbolF s) index = index C.:< Ast0.SymbolF s
    go (Ast0.CompoundF xs) index = index C.:< Ast0.CompoundF (zipWith (. snoc index) xs [0 ..])

-- Replaces the node at 'index' with 'replacement' in 'ast'. No
-- replacement is made if 'index' is invalid.
replace0At :: Ast0.Ast -> [Int] -> Ast0.Ast -> Ast0.Ast
replace0At ast index replacement = cata go (index0 ast)
  where
    go :: CofreeF Ast0.AstF [Int] Ast0.Ast -> Ast0.Ast
    go (i :< t) =
      if i == index
        then replacement
        else embed t
