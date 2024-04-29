{-# LANGUAGE DeriveGeneric #-}

module Compile
  ( compile,
    compile0toRuleDefinition,
    ruleDefinitionPredicates,
    ruleDefinitionVariableBindings,
    compile0to1,
    compile1toP0,
    compile1toC0,
    compileC0toC1,
    compileC1toStmts,
    compileRule,
    Variables,
    RuleDefinition (..),
    compileRule2,
  )
where

import qualified Ast0
import qualified Ast1
import qualified AstC0
import qualified AstC1
import qualified AstC1P
import qualified AstC2
import qualified AstC2ConstExpr
import qualified AstC2Expr
import AstP0 (indexP0ByC0)
import qualified AstP0
import ConstantExpr (ConstantExpr (..))
import Control.Comonad (Comonad (..))
import Control.Comonad.Cofree (Cofree, ComonadCofree (unwrap))
import qualified Control.Comonad.Cofree as CCC
import Control.Comonad.Trans.Cofree (CofreeF (..))
import Control.Monad (foldM)
import Control.Monad.State.Strict
  ( State,
    gets,
    modify,
    runState,
  )
import Data.Either.Extra (maybeToEither)
import Data.Functor.Foldable (ListF (..), Recursive (..), histo)
import Data.HashMap.Strict ((!?))
import qualified Data.HashMap.Strict as H
import Data.Hashable (Hashable)
import Data.List (uncons)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Debug.Trace (trace)
import Display (displayC0)
import Error (CompileError (..), CompileResult)
import qualified Expr
import GHC.Generics (Generic)
import qualified Op
import Predicate (IndexedPredicate (..), Predicate (LengthEqualTo, LengthGreaterThanOrEqualTo, SymbolEqualTo))
import Stmt (Stmt (..))
import Var (Var)

type Variables = H.HashMap String AstC0.Index

compile :: Variables -> Ast0.Ast -> CompileResult [Stmt Int]
compile vars ast = do
  let ast1 = compile0to1 ast
  let astc0 = compile1toC0 vars ast1
  astc1 <- compileC0toC1 astc0
  let stmtsWithNamedLabels = compileC1toStmts astc1
  let stmtsWithOffsetLabels = compileLabels stmtsWithNamedLabels
  return stmtsWithOffsetLabels

-- Finds the first element in a list that satisfies a predicate,
-- returning the elements before it, itself, and the elements that
-- follow it. Nothing is returned if no element satisfies the predicate.
splitBeforeAndAfter :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
splitBeforeAndAfter p = go []
  where
    go acc (x : xs)
      | p x = Just (reverse acc, x, xs)
      | otherwise = go (x : acc) xs
    go _ [] = Nothing

compile1toP0 :: Ast1.Ast -> CompileResult AstP0.Ast
compile1toP0 = histo go
  where
    go :: Ast1.AstF (Cofree Ast1.AstF (CompileResult AstP0.Ast)) -> CompileResult AstP0.Ast
    go = \case
      Ast1.SymbolF s -> Right $ AstP0.Symbol s
      Ast1.CompoundF xs ->
        let wasEllipses :: Cofree Ast1.AstF a -> Bool
            wasEllipses (_ CCC.:< Ast1.EllipsesF _) = True
            wasEllipses _ = False
            xsSplit = splitBeforeAndAfter wasEllipses xs
         in case xsSplit of
              Nothing -> do
                xs' <- mapM extract xs
                Right $ AstP0.CompoundWithoutEllipses xs'
              Just (b, e, a) ->
                if any wasEllipses a
                  then Left MoreThanOneEllipsisInSingleCompoundTermOfPattern
                  else do
                    b' <- mapM extract b
                    e' <- extract e
                    a' <- mapM extract a
                    Right $ AstP0.CompoundWithEllipses b' e' a'
      Ast1.EllipsesF x -> extract x

data RuleDefinition = RuleDefinition
  { _variables :: ![String],
    _pattern :: !AstP0.Ast,
    _constructor :: !Ast0.Ast
  }

compileRule2 :: RuleDefinition -> CompileResult ([IndexedPredicate], AstC2.Ast Int)
compileRule2 rule@(RuleDefinition _ _ constructor) = do
  vars <- ruleDefinitionVariableBindings rule
  preds <- ruleDefinitionPredicates rule
  program <- compile2 vars constructor
  Right (preds, program)

compile2 :: a
compile2 = undefined

compileRule :: RuleDefinition -> CompileResult ([IndexedPredicate], [Stmt Int])
compileRule rule@(RuleDefinition _ _ constructor) = do
  vars <- ruleDefinitionVariableBindings rule
  preds <- ruleDefinitionPredicates rule
  stmts <- compile vars constructor
  Right (preds, stmts)

compile0toRuleDefinition :: Ast0.Ast -> CompileResult RuleDefinition
compile0toRuleDefinition (Ast0.Symbol _) = Left InvalidRuleDefinition
compile0toRuleDefinition (Ast0.Compound xs) =
  -- rules must have at least 4 subterms:
  --  1   2 3  4
  -- (def a -> b)
  --
  -- they can have as many variables as necessary before the pattern 'a':
  -- (def var1 var2 var3 a -> b)
  if length xs < 4
    then Left InvalidRuleDefinition
    else
      let (<&&>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
          (<&&>) f g x = f x && g x

          isValidRuleSyntax :: [Ast0.Ast] -> Bool
          isValidRuleSyntax =
            startsWithDefSymbol
              <&&> hasArrowBetweenPatternAndConstructor
              <&&> allVariablesAreSymbols

          startsWithDefSymbol :: [Ast0.Ast] -> Bool
          startsWithDefSymbol (Ast0.Symbol "def" : _) = True
          startsWithDefSymbol _ = False

          hasArrowBetweenPatternAndConstructor :: [Ast0.Ast] -> Bool
          hasArrowBetweenPatternAndConstructor ast =
            let arrow = ast !! (length ast - 2)
             in arrow == Ast0.Symbol "->"

          allVariablesAreSymbols :: [Ast0.Ast] -> Bool
          allVariablesAreSymbols ast =
            let isSymbol :: Ast0.Ast -> Bool
                isSymbol (Ast0.Symbol _) = True
                isSymbol _ = False
             in all isSymbol $ ruleDefinitionVariables ast

          ruleDefinitionVariables :: [Ast0.Ast] -> [Ast0.Ast]
          ruleDefinitionVariables ast =
            let astWithoutPredicateAndArrowAndConstructor = take (length ast - 3) ast
                astWithoutDefSymbol = drop 1 astWithoutPredicateAndArrowAndConstructor
             in astWithoutDefSymbol

          ruleDefinitionPattern :: [Ast0.Ast] -> Ast0.Ast
          ruleDefinitionPattern ast = ast !! (length ast - 3)

          ruleDefinitionConstructor :: [Ast0.Ast] -> Ast0.Ast
          ruleDefinitionConstructor ast = ast !! (length ast - 1)
       in if isValidRuleSyntax xs
            then
              let symbolToString :: Ast0.Ast -> String
                  symbolToString (Ast0.Symbol s) = s
                  symbolToString _ = error "not a symbol"

                  vars = map symbolToString $ ruleDefinitionVariables xs
                  pat = compile0to1 $ ruleDefinitionPattern xs
                  constr = ruleDefinitionConstructor xs
               in do
                    pat' <- compile1toP0 pat
                    Right $ RuleDefinition vars pat' constr
            else Left InvalidRuleDefinition

-- Returns the union of all hashmaps in the input list, or Nothing if there
-- exists at least one key present in more than of the hashmaps.
unionNonIntersectingHashMaps :: (Hashable k) => [H.HashMap k v] -> Maybe (H.HashMap k v)
unionNonIntersectingHashMaps hs =
  let keyCountBeforeUnion = sum $ map (length . H.keys) hs
      union = H.unions hs
      keyCountAfterUnion = length $ H.keys union
   in if keyCountBeforeUnion == keyCountAfterUnion
        then Just union
        else Nothing

-- Returns a hashmap of variable names to their locations in the pattern
-- of the given rule definition.
--
-- For example, in the following rule:
--
--  (def x y (0 1 x 3 4 (50 51 y)) -> result)
--
-- the variable 'x' is located at index [2], and the variable 'y' is
-- located at index [5, 2]. Returns a compile error instead if any variable
-- occurs more than once in the pattern.
ruleDefinitionVariableBindings :: RuleDefinition -> CompileResult Variables
ruleDefinitionVariableBindings (RuleDefinition vars pat _) =
  cata go (indexP0ByC0 pat)
  where
    go ::
      CofreeF
        AstP0.AstF
        AstC0.Index
        (CompileResult Variables) ->
      CompileResult Variables
    go (index :< ast) = case ast of
      AstP0.SymbolF s ->
        Right $
          if s `elem` vars
            then H.singleton s index
            else H.empty
      AstP0.CompoundWithoutEllipsesF xs -> do
        xs' <- sequence xs
        let combined = unionNonIntersectingHashMaps xs'
        maybeToEither VariableUsedMoreThanOnceInPattern combined
      AstP0.CompoundWithEllipsesF b e a -> do
        b' <- sequence b
        e' <- e
        a' <- sequence a
        let combined = unionNonIntersectingHashMaps $ e' : (b' ++ a')
        maybeToEither VariableUsedMoreThanOnceInPattern combined

-- Returns a list of conditions that must hold for a given rule's pattern to
-- match a term.
--
-- For example, in the following rule:
--
--   (def xs (flatten (list (list xs ..) ..)) -> (list xs .. ..))
--
-- the following conditions must hold if the rule is to match a given term:
--
-- - Index [] is a compound term of length == 2
-- - Index [0] == "flatten"
-- - Index [1] is a compound term of length >= 1
-- - Index [1,0] == "list"
-- - Indices [1,1..length] are compound terms of length >= 1
-- - Indices [1,1..length,0] == "list"
ruleDefinitionPredicates :: RuleDefinition -> CompileResult [IndexedPredicate]
ruleDefinitionPredicates (RuleDefinition vars pat _) = cata go (indexP0ByC0 pat)
  where
    go ::
      CofreeF AstP0.AstF AstC0.Index (CompileResult [IndexedPredicate]) ->
      CompileResult [IndexedPredicate]
    go (index :< ast) = case ast of
      AstP0.SymbolF s ->
        Right [IndexedPredicate (SymbolEqualTo s) index | s `notElem` vars]
      AstP0.CompoundWithoutEllipsesF xs -> do
        xs' <- concat <$> sequence xs
        let p = IndexedPredicate (LengthEqualTo (length xs)) index
        pure $ p : xs'
      AstP0.CompoundWithEllipsesF b e a -> do
        b' <- concat <$> sequence b
        e' <- e
        a' <- concat <$> sequence a
        let p = IndexedPredicate (LengthGreaterThanOrEqualTo $ length b + length a) index
        pure $ p : (b' ++ e' ++ a')

compile0to1 :: Ast0.Ast -> Ast1.Ast
compile0to1 = cata $ \case
  Ast0.SymbolF s -> Ast1.Symbol s
  Ast0.CompoundF xs -> Ast1.Compound $ go xs
    where
      go :: [Ast1.Ast] -> [Ast1.Ast]
      go (y : Ast1.Symbol ".." : ys) = go $ Ast1.Ellipses y : ys
      go (y : ys) = y : go ys
      go [] = []

compile1toC0 :: Variables -> Ast1.Ast -> AstC0.Ast
compile1toC0 vars = cata $ \case
  Ast1.SymbolF s -> case H.lookup s vars of
    Nothing -> AstC0.Symbol s
    Just index -> AstC0.Variable index
  Ast1.CompoundF xs -> AstC0.Compound xs
  Ast1.EllipsesF x -> AstC0.Ellipses x

allEqualOrEmpty :: (Eq a) => [[a]] -> Bool
allEqualOrEmpty = go . filter (not . null)
  where
    go [] = True
    go (x : xs) = all (== x) xs

combineCompoundTermIndices :: [Maybe AstC0.Index] -> CompileResult (Maybe AstC0.Index)
combineCompoundTermIndices xs =
  let xs' = catMaybes xs
   in if allEqualOrEmpty xs'
        then Right (fst <$> uncons (filter (not . null) xs'))
        else Left Error.VarsNotCapturedUnderSameEllipsisInConstructor

type ListF' a t = ListF a t -> t

compileC0toC1P :: AstC0.Ast -> CompileResult (AstC1P.Ast, H.HashMap Var AstC0.Index)
compileC0toC1P ast = do
  (ast, h, _) <- cata go ast 0
  undefined
  where
    go :: AstC0.AstF' (Var -> CompileResult (AstC1P.Ast, H.HashMap AstC0.Index Var, Var))
    go ast nextVar = case ast of
      AstC0.SymbolF s -> Right (AstC1P.Symbol s, H.empty, nextVar)
      AstC0.CompoundF xs ->
        let f ::
              ListF'
                ( Var ->
                  CompileResult (AstC1P.Ast, H.HashMap AstC0.Index Var, Var)
                )
                ( CompileResult
                    ( [(AstC1P.Ast, H.HashMap AstC0.Index Var)],
                      Var
                    )
                )
            f Nil = Right ([], nextVar)
            f (Cons x result) = do
              (xs, var) <- result
              (ast, h, var') <- x var
              pure $ ((ast, h) : xs, var')
            xs' = cata f xs
         in do
              (xs', nextVar') <- cata f xs
              undefined
      AstC0.EllipsesF x -> undefined
      AstC0.VariableF x -> undefined

compileC1PToC2 :: AstC1P.Ast -> AstC2.Ast NamedLabel
compileC1PToC2 = histo go
  where
    go ::
      AstC1P.AstF (Cofree AstC1P.AstF (AstC2.Ast NamedLabel)) ->
      AstC2.Ast NamedLabel
    go = \case
      AstC1P.SymbolF s -> undefined -- [AstC2.Push $ AstC2ConstExpr.Symbol s]
      AstC1P.CopyF i -> undefined -- cata convert i
        where
          convert :: ListF' AstC1P.IndexElement [AstC2.Stmt NamedLabel]
          convert Nil = []
          convert (Cons indexElement stmts) = case indexElement of
            AstC1P.ZeroPlus zp -> undefined
            AstC1P.LenMinus lm -> undefined
            AstC1P.Var v -> undefined
      AstC1P.CompoundF xs -> undefined
      AstC1P.LoopF var src start end body -> undefined

-- go nextVar = \case
--   AstC0.SymbolF s -> Right (AstC1P.Symbol s, H.empty)
--   AstC0.CompoundF xs -> do
--     xs' <- sequence xs
--     undefined
--   AstC0.EllipsesF x -> undefined
--   AstC0.VariableF i -> Right $ AstC1P.Copy $
--     undefined

compileC0toC1 :: AstC0.Ast -> CompileResult AstC1.Ast
compileC0toC1 astC0 = do
  (astC1, index) <- cata go $ trace (displayC0 astC0) astC0
  case index of
    Nothing -> Right astC1
    Just [] -> Right astC1
    Just _ -> Left Error.TooFewEllipsesInConstructor
  where
    go :: AstC0.AstF (CompileResult (AstC1.Ast, Maybe AstC0.Index)) -> CompileResult (AstC1.Ast, Maybe AstC0.Index)
    go (AstC0.SymbolF s) = Right (AstC1.Symbol s, Nothing)
    go (AstC0.CompoundF xs) = do
      xs' <- sequence xs
      index <- combineCompoundTermIndices (map snd xs')
      return (AstC1.Compound $ map fst xs', index)
    go (AstC0.VariableF i) =
      let (c0Part, c1Part) = AstC0.popTrailingC1Index i
       in Right (AstC1.Copy c1Part, Just c0Part)
    go (AstC0.EllipsesF term) = trace (show $ show <$> term) $ do
      (astC1, indexC0) <- term
      case indexC0 of
        Nothing ->
          case astC1 of
            AstC1.Symbol _ ->
              Left Error.EllipsisAppliedToSymbolInConstructor
            _other -> error "unreachable, possibly incorrect var bindings"
        Just indexC0' ->
          case AstC0.popBetweenTail (trace (show indexC0') indexC0') of
            (indexC0'', Just (zeroPlus, lenMinus)) ->
              let (fstC0, sndC1) = AstC0.popTrailingC1Index indexC0''
                  loopC1 =
                    AstC1.Loop
                      { AstC1.index = sndC1,
                        AstC1.start = zeroPlus,
                        AstC1.end = lenMinus,
                        AstC1.body = astC1
                      }
               in Right (loopC1, Just fstC0)
            ([], Nothing) -> undefined
            (_, Nothing) -> Left Error.TooManyEllipsesInConstructor

newUniqueVar :: State C1ToStmtsState Var
newUniqueVar = do
  var <- gets currentVar
  modify incVar
  return var

pushIndexToStackStmts :: AstC1.Index -> State C1ToStmtsState [Stmt a]
pushIndexToStackStmts = cata $ \case
  Nil -> return []
  Cons (AstC1.ZeroPlus zp) stmts -> do
    stmts' <- stmts
    return $ Stmt.PushIndexToIndexStack (Constant zp) : stmts'
  Cons (AstC1.LenMinus lm) stmts -> do
    stmts' <- stmts
    var <- newUniqueVar
    let assign = Stmt.Assign {lhs = var, rhs = Expr.Length}
    let sub =
          Stmt.Assign
            { lhs = var,
              rhs =
                Expr.BinOp
                  { Expr.op = Op.Sub,
                    Expr.lhs = var,
                    Expr.rhs = ConstantExpr.Constant lm
                  }
            }
    let push = Stmt.PushIndexToIndexStack $ ConstantExpr.Var var
    return $ [assign, sub, push] ++ stmts'

popFromIndexStackStmt :: AstC1.Index -> Stmt a
popFromIndexStackStmt =
  Stmt.PopFromIndexStack . length

data C1ToStmtsState = C1ToStmtsState
  { currentVar :: !Var,
    iterationCountVar :: !(Maybe Var)
  }

incVar :: C1ToStmtsState -> C1ToStmtsState
incVar state = state {currentVar = currentVar state + 1}

setIterationCountVar :: C1ToStmtsState -> C1ToStmtsState
setIterationCountVar state = state {currentVar = currentVar state + 1, iterationCountVar = Just $ currentVar state}

initialC1ToStmtsState :: C1ToStmtsState
initialC1ToStmtsState = C1ToStmtsState {currentVar = 0, iterationCountVar = Nothing}

data NamedLabel = TopOfLoop !Int | BotOfLoop !Int deriving (Eq, Generic)

instance Hashable NamedLabel

compileC1toStmts :: AstC1.Ast -> [Stmt NamedLabel]
compileC1toStmts = fst . flip runState initialC1ToStmtsState . histo go
  where
    shouldIncrementIterationCount :: Cofree AstC1.AstF (State C1ToStmtsState [Stmt NamedLabel]) -> Bool
    shouldIncrementIterationCount (_ CCC.:< (AstC1.LoopF {})) = False
    shouldIncrementIterationCount _ = True

    isLoopF :: AstC1.AstF a -> Bool
    isLoopF (AstC1.LoopF {}) = True
    isLoopF _ = False

    go :: AstC1.AstF (Cofree AstC1.AstF (State C1ToStmtsState [Stmt NamedLabel])) -> State C1ToStmtsState [Stmt NamedLabel]
    go = \case
      AstC1.SymbolF s -> return [PushSymbolToDataStack s]
      AstC1.CompoundF xs -> do
        let g = map extract xs :: [State C1ToStmtsState [Stmt NamedLabel]]
            orig = map unwrap xs
            numLoopyBodies = length $ filter isLoopF orig
            numNonLoopyBodies = length orig - numLoopyBodies
        modify setIterationCountVar
        count <- gets iterationCountVar
        xs' <- sequence g
        case count of
          Nothing -> error "no iteration counter"
          Just count_var ->
            return $
              Stmt.Assign {lhs = count_var, rhs = Expr.Constant numNonLoopyBodies}
                : concat xs'
                ++ [BuildCompoundTermFromDataStack {term_count = ConstantExpr.Var count_var}]
      AstC1.CopyF i -> do
        pushStackStmts <- pushIndexToStackStmts i
        return $ pushStackStmts ++ [Stmt.PushIndexedTermToDataStack, popFromIndexStackStmt i]
      AstC1.LoopF index start end body -> do
        loopVar <- newUniqueVar
        lengthVar <- newUniqueVar
        endVar <- newUniqueVar
        loopLabel <- newUniqueVar

        pushStackStmts <- pushIndexToStackStmts index

        count <- gets iterationCountVar

        case count of
          Nothing -> error "uncreachable: no iteration counter"
          Just count_var -> do
            let inc_stmt =
                  ( [ Stmt.Assign
                        { lhs = count_var,
                          rhs =
                            Expr.BinOp
                              { Expr.op = Op.Add,
                                Expr.lhs = count_var,
                                Expr.rhs = ConstantExpr.Constant 1
                              }
                        }
                      | shouldIncrementIterationCount body
                    ]
                  ) ::
                    [Stmt NamedLabel]
            let pushLoopVar = Stmt.PushIndexToIndexStack (ConstantExpr.Var loopVar)
            let popLoopVar = Stmt.PopFromIndexStack 1

            body' <- extract body

            let body'' = pushLoopVar : body' ++ [popLoopVar]

            let prologue =
                  [Stmt.Assign {lhs = loopVar, rhs = Expr.Constant start}]
                    ++ pushStackStmts
                    ++ [ Stmt.Assign {lhs = lengthVar, rhs = Expr.Length},
                         Stmt.Assign {lhs = endVar, rhs = Expr.Constant end},
                         Stmt.Assign
                           { lhs = endVar,
                             rhs =
                               Expr.BinOp
                                 { Expr.op = Op.Sub,
                                   Expr.lhs = lengthVar,
                                   Expr.rhs = ConstantExpr.Var endVar
                                 }
                           },
                         Stmt.Jump $ TopOfLoop loopLabel
                       ]
            let epilogue =
                  [ Stmt.Assign
                      { lhs = loopVar,
                        rhs =
                          Expr.BinOp
                            { Expr.op = Op.Add,
                              Expr.lhs = loopVar,
                              Expr.rhs = ConstantExpr.Constant 1
                            }
                      }
                  ]
                    ++ inc_stmt
                    ++ [ Stmt.JumpWhenLessThan
                           { label = BotOfLoop loopLabel,
                             when_var = loopVar,
                             le_var = endVar
                           },
                         popFromIndexStackStmt index
                       ]
            let result = prologue ++ body'' ++ epilogue
            return result

compileLabels :: [Stmt NamedLabel] -> [Stmt Int]
compileLabels xs = go (calculateInstructionNumbers (zip [0 ..] xs)) xs
  where
    calculateInstructionNumbers :: [(Int, Stmt NamedLabel)] -> H.HashMap NamedLabel Int
    calculateInstructionNumbers =
      H.fromList
        . mapMaybe
          ( \case
              (offset, Jump l) -> Just (l, offset)
              (offset, JumpWhenLessThan l _ _) -> Just (l, offset)
              (_, _) -> Nothing
          )
    go :: H.HashMap NamedLabel Int -> [Stmt NamedLabel] -> [Stmt Int]
    go ht = cata $ \case
      Nil -> []
      Cons stmt others -> stmt' : others
        where
          stmt' = fmap namedInstructionNumber stmt

          namedInstructionNumber :: NamedLabel -> Int
          namedInstructionNumber (TopOfLoop l) = fromJust $ ht !? BotOfLoop l
          namedInstructionNumber (BotOfLoop l) = 1 + fromJust (ht !? TopOfLoop l)
