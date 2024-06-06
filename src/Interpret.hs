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
import Control.Monad.ST (ST, runST)
import Data.Array (Array, listArray)
import Data.Array.ST (MArray (newArray_), STArray, readArray, writeArray)
import Data.Foldable (find)
import Data.Functor.Foldable (ListF (..), cata)
import Data.Graph.Inductive (Node, context, labNode', lsuc)
import Data.Sequence (Seq (..), fromList, singleton)
import Predicate (applyPredicates)
import Utils (Cata, iterateMaybe)
import Data.Array.Base ((!?))

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

iterateMaybeST :: forall s b. (b -> ST s (Maybe b)) -> b -> ST s [b]
iterateMaybeST f b =
  (b :) <$> do
    fb <- f b
    case fb of
      Nothing -> pure []
      Just b -> iterateMaybeST f b

listToArray :: [e] -> Array Int e
listToArray xs = listArray (0, length xs - 1) xs

runConstructor :: AstC2.Ast Int -> Ast0.Ast -> Ast0.Ast
runConstructor constructor input =
  head $ last allDataStacks
  where
    allDataStacks :: [[Ast0.Ast]]
    allDataStacks = runST allDataStacksST

    allDataStacksST :: ST s [[Ast0.Ast]]
    allDataStacksST = fmap (map dataStack) allStatesST

    allStatesST :: ST s [Memory s]
    allStatesST = do
      initialState <- initialState
      iterateMaybeST interpretNextInstruction initialState

    initialState :: ST s (Memory s)
    initialState = do
      variables <- newArray_ (0, largestVariable constructor)
      pure $
        Memory
          { input = input,
            program = listToArray constructor,
            instruction = 0,
            dataStack = [],
            variables
          }

    interpretNextInstruction :: Memory s -> ST s (Maybe (Memory s))
    interpretNextInstruction m =
      case m of
        Memory
          { input = _input,
            program = program,
            instruction = instruction,
            dataStack = dataStack,
            variables = variables
          } -> do
            let maybeI = program !? instruction
            case maybeI of
              Nothing -> pure Nothing
              Just i -> case i of
                AstC2.Assign (AstC2Assign.Assign lhs rhs) -> do
                  e <- evalExpr m rhs
                  writeArray variables lhs e
                  pure . Just $
                    m
                      { variables,
                        instruction = instruction + 1
                      }
                AstC2.Push expr -> do
                  expr' <- evalExpr m expr
                  let astExpr = Value.expectAst expr'
                  pure . Just $
                    m
                      { dataStack = astExpr : dataStack,
                        instruction = instruction + 1
                      }
                AstC2.Build termCount -> do
                  termCount' <- evalExpr m termCount
                  let termCountNat = Value.expectNat termCount'
                      newTerm = Ast0.Compound . reverse $ take termCountNat dataStack
                  pure . Just $
                    m
                      { dataStack = newTerm : drop termCountNat dataStack,
                        instruction = instruction + 1
                      }
                AstC2.Jump (AstC2Jump.Jump target condition) -> do
                  condition' <- evalExpr m condition
                  let conditionBool = Value.expectBool condition'
                      nextInstruction =
                        if conditionBool
                          then target
                          else instruction + 1
                  pure . Just $ m {instruction = nextInstruction}

largestVariable :: AstC2.Ast Int -> Var
largestVariable = cata go
  where
    go :: Cata (AstC2.Ast Int) Var
    go = \case
      Nil -> 0
      Cons ast result -> max result (extractVar ast)

    extractVar :: AstC2.Stmt Int -> Var
    extractVar = \case
      AstC2.Assign (AstC2Assign.Assign {lhs}) -> lhs
      _ -> 0

evalExpr :: forall s. Memory s -> Expr -> ST s Value
evalExpr m = cata go
  where
    go :: Cata Expr (ST s Value)
    go = \case
      Expr.BoolF b -> pure $ Value.Bool b
      Expr.VarF v -> evalVar m v
      Expr.NatF n -> pure $ Value.Nat n
      Expr.SymbolF s -> pure $ Value.Ast $ Ast0.Symbol s
      Expr.InputF -> pure $ Value.Ast m.input
      Expr.BinOpF op lhs rhs -> do
        lhs <- lhs
        rhs <- rhs
        case op of
          Expr.Add ->
            let lhsNat = Value.expectNat lhs
                rhsNat = Value.expectNat rhs
             in pure $ Value.Nat $ lhsNat + rhsNat
          Expr.Sub ->
            let lhsNat = Value.expectNat lhs
                rhsNat = Value.expectNat rhs
             in pure $ Value.Nat $ lhsNat - rhsNat
          Expr.ArrayAccess ->
            let lhsAst = Value.expectAst lhs
                rhsNat = Value.expectNat rhs
             in case lhsAst of
                  Ast0.Symbol _ -> Value.mkTypeError "Compound" lhs
                  Ast0.Compound xs -> pure $ Value.Ast $ xs !! rhsNat
          Expr.LessThan ->
            let lhsNat = Value.expectNat lhs
                rhsNat = Value.expectNat rhs
             in pure $ Value.Bool $ lhsNat < rhsNat
      Expr.LengthF e -> do
        e <- e
        case Value.expectAst e of
          Ast0.Symbol _ -> Value.mkTypeError "Compound" e
          Ast0.Compound xs -> pure $ Value.Nat $ length xs

evalVar :: Memory s -> Var -> ST s Value
evalVar m = readArray m.variables

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

data Memory s = Memory
  { input :: !Ast0.Ast,
    program :: !(Array Int (AstC2.Stmt Int)),
    instruction :: !Int,
    dataStack :: ![Ast0.Ast],
    variables :: !(STArray s Int Value)
  }