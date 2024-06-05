{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Interpret
  ( interpret,
    Reduction,
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
import Cfg (Cfg (..))
import Control.DeepSeq (force)
import Data.Foldable (find)
import Data.Functor.Foldable (cata)
import Data.Graph.Inductive (Node, context, labNode', lsuc)
import Data.List.Extra ((!?))
import Data.Sequence (Seq (..), fromList, singleton)
import Predicate (applyPredicates)
import Utils (Cata, iterateMaybe, setNth)

-- The stream of results of interpreting some input. The first element
-- is the input, the next element is the result of applying a definition
-- to the input, the third is the result of applying a definition to the
-- second, and so on. The last element is the one to which there are no
-- further definitions left to apply.
type Reduction = [Ast0.Ast]

interpret :: Cfg -> Ast0.Ast -> Reduction
interpret cfg = iterateMaybe (force . applyOneDefinitionBFS cfg)

-- | Recursively searches through 'ast' from the top to bottom in a breadth-first-search order,
-- applying and returning the result of the first matching definition from 'cfg'. Returns
-- 'Nothing' if no such definition exists.
applyOneDefinitionBFS :: Cfg -> Ast0.Ast -> Maybe Ast0.Ast
applyOneDefinitionBFS cfg ast = go $ singleton $ Matcher cfg.start ast []
  where
    go :: Seq Matcher -> Maybe Ast0.Ast
    go matcherQueue =
      case matcherQueue of
        Empty -> Nothing
        matcher :<| matcherQueue ->
          case applyOneDefinition cfg matcher of
            Left subtermMatchers ->
              go $ matcherQueue <> subtermMatchers
            Right matcher ->
              case _ast matcher of
                replacementAst ->
                  Just $ replace0At ast matcher.index replacementAst

-- | Returns a single matcher holding the result of successfully applying a definition to the
-- ast in the input matcher. Otherwise, if no definition applies to the ast. returns a list
-- of matchers holding subterms of the ast so they may be later tried in a breadth-first
-- search order.
applyOneDefinition :: Cfg -> Matcher -> Either (Seq Matcher) Matcher
applyOneDefinition cfg matcher =
  let neighbors = lsuc cfg.graph matcher._node
      maybeNextNode = fst <$> find (\(_, preds) -> applyPredicates preds matcher._ast) neighbors
   in case maybeNextNode of
        Just nextNode ->
          let constructor = snd $ labNode' $ context cfg.graph nextNode
              nextAst = runConstructor constructor matcher._ast
              nextNodeNeighbors = lsuc cfg.graph nextNode
              newNode =
                if null nextNodeNeighbors
                  then cfg.start
                  else nextNode
              currentIndex = matcher.index
           in Right $ Matcher newNode nextAst currentIndex
        Nothing -> Left $ fromList $ case matcher._ast of
          Ast0.Symbol _ -> []
          Ast0.Compound xs ->
            zipWith m1 [0 ..] xs
            where
              m1 :: Int -> Ast0.Ast -> Matcher
              m1 i x = Matcher cfg.start x $ matcher.index <> [i]

runConstructor :: AstC2.Ast Int -> Ast0.Ast -> Ast0.Ast
runConstructor constructor input =
  head . dataStack . last $
    iterateMaybe transition initialState
  where
    initialState :: Memory
    initialState =
      Memory
        { input = input,
          program = constructor,
          instruction = 0,
          dataStack = [],
          variables = []
        }

    transition :: Memory -> Maybe Memory
    transition m = case m of
      Memory
        { input = _input,
          program = program,
          instruction = instruction,
          dataStack = dataStack,
          variables = variables
        } -> do
          i <- program !? instruction
          pure $ case i of
            AstC2.Assign (AstC2Assign.Assign lhs rhs) ->
              m
                { variables =
                    setNth
                      lhs
                      (\var -> error $ "$" ++ show var ++ " is undefined")
                      (evalExpr m rhs)
                      variables,
                  instruction = instruction + 1
                }
            AstC2.Push expr ->
              let expr' = evalExpr m expr
                  astExpr = Value.expectAst expr'
               in m
                    { dataStack = astExpr : dataStack,
                      instruction = instruction + 1
                    }
            AstC2.Build termCount ->
              let termCount' = evalExpr m termCount
                  termCountNat = Value.expectNat termCount'
                  newTerm = Ast0.Compound . reverse $ take termCountNat dataStack
               in m
                    { dataStack = newTerm : drop termCountNat dataStack,
                      instruction = instruction + 1
                    }
            AstC2.Jump (AstC2Jump.Jump target condition) ->
              let condition' = evalExpr m condition
                  conditionBool = Value.expectBool condition'
                  nextInstruction =
                    if conditionBool
                      then target
                      else instruction + 1
               in m {instruction = nextInstruction}

evalExpr :: Memory -> Expr -> Value
evalExpr m = cata go
  where
    go :: Cata Expr Value
    go = \case
      Expr.BoolF b -> Value.Bool b
      Expr.VarF v -> evalVar m v
      Expr.NatF n -> Value.Nat n
      Expr.SymbolF s -> Value.Ast $ Ast0.Symbol s
      Expr.InputF -> Value.Ast m.input
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
evalVar m v = m.variables !! v

replace0At :: Ast0.Ast -> [Int] -> Ast0.Ast -> Ast0.Ast
replace0At ast index replacement = case index of
  [] -> replacement
  n : index -> case ast of
    Ast0.Symbol _ -> error "replace0At: out of bounds index"
    Ast0.Compound xs -> Ast0.Compound xs'
      where
        xs' = before ++ [x] ++ after
        before = take n xs
        after = drop (n + 1) xs
        x = replace0At (xs !! n) index replacement

data Matcher = Matcher
  { _node :: !Node,
    _ast :: !Ast0.Ast,
    index :: [Int]
  }

data Memory = Memory
  { input :: !Ast0.Ast,
    program :: !(AstC2.Ast Int),
    instruction :: !Int,
    dataStack :: ![Ast0.Ast],
    variables :: ![Value]
  }
  deriving (Show)