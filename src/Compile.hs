{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Compile
  ( -- compile,
    compileC2,
    compile0toRuleDefinition,
    ruleDefinitionPredicates,
    -- ruleDefinitionVariableBindings,
    compile0to1,
    compile1toP0,
    compile1toC0,
    -- compileC0toC1,
    -- compileC1toStmts,
    -- compileRule,
    VariableBindings,
    RuleDefinition (..),
    compileRule2,
    compileC0ToC1P,
    C0ToC1Data (..),
  )
where

import qualified Ast0
import qualified Ast1
import qualified AstC0
import AstC1P (AssignmentLocation (..))
import qualified AstC1P
import qualified AstC2
import qualified AstC2Assign
import qualified AstC2Expr
import qualified AstC2Expr as C2Expr
import qualified AstC2Jump
import AstP0 (indexP0ByC0)
import qualified AstP0
import Control.Comonad (Comonad (..))
import Control.Comonad.Cofree (Cofree)
import qualified Control.Comonad.Cofree as CCC
import Control.Comonad.Trans.Cofree (CofreeF (..))
import Control.Monad.State.Strict
  ( State,
    evalState,
    gets,
    modify,
  )
import Data.Either.Extra (maybeToEither)
import Data.Functor.Foldable (ListF (..), Recursive (..), histo)
import Data.HashMap.Strict ((!?))
import qualified Data.HashMap.Strict as H
import Data.Hashable (Hashable)
import Data.Maybe (fromJust)
import Error (CompileError (..), CompileResult)
import GHC.Generics (Generic)
import Predicate (IndexedPredicate (..), Predicate (LengthEqualTo, LengthGreaterThanOrEqualTo, SymbolEqualTo))
import Utils (Between (..), Cata, Histo, Para, popBetweenTail, popTrailingC1Index)
import Var (Var)

type VariableBindings = H.HashMap String AstC0.Index

compileC2 :: VariableBindings -> Ast0.Ast -> CompileResult (AstC2.Ast Int)
compileC2 vars ast = do
  let ast1 = compile0to1 ast
      astC0 = compile1toC0 vars ast1
  (astC1P, nextUnusedVar) <- compileC0ToC1P astC0
  let namedC2Stmts = compileC1PToC2 nextUnusedVar astC1P
      offsetC2Stmts = resolveC2NamedLabels namedC2Stmts
  pure offsetC2Stmts

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
compile1toP0 = para go
  where
    go :: Para Ast1.Ast (CompileResult AstP0.Ast)
    go = \case
      Ast1.SymbolF s -> Right $ AstP0.Symbol s
      Ast1.CompoundF inputXsPairs ->
        let input :: [Ast1.Ast]
            input = map fst inputXsPairs
            xs :: [CompileResult AstP0.Ast]
            xs = map snd inputXsPairs
            wasEllipses :: (Ast1.Ast, AstP0.Ast) -> Bool
            wasEllipses = \case
              (Ast1.Ellipses _, _) -> True
              _ -> False
         in do
              inputXsPairs <- zip input <$> sequence xs
              let inputXsPairsSplit = splitBeforeAndAfter wasEllipses inputXsPairs
              case inputXsPairsSplit of
                Nothing ->
                  Right $ AstP0.CompoundWithoutEllipses $ map snd inputXsPairs
                Just (b, e, a) ->
                  if any wasEllipses a
                    then Left MoreThanOneEllipsisInSingleCompoundTermOfPattern
                    else Right $ AstP0.CompoundWithEllipses (map snd b) (snd e) (map snd a)
      Ast1.EllipsesF x -> extract x

data RuleDefinition = RuleDefinition
  { _variables :: !VariableBindings,
    _pattern :: !AstP0.Ast,
    _constructor :: !Ast0.Ast
  }

isDollarSignVar :: String -> Bool
isDollarSignVar ('$' : _) = True
isDollarSignVar _ = False

p0VariableBindings :: AstP0.Ast -> CompileResult VariableBindings
p0VariableBindings = cata go . indexP0ByC0
  where
    go :: Cata (Cofree AstP0.AstF AstC0.Index) (CompileResult VariableBindings)
    go (index :< ast) = case ast of
      AstP0.SymbolF s ->
        Right $
          if isDollarSignVar s
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

compileRule2 :: RuleDefinition -> CompileResult ([IndexedPredicate], AstC2.Ast Int)
compileRule2 rule@(RuleDefinition vars pattern constructor) = do
  preds <- ruleDefinitionPredicates rule
  program <- compileC2 vars constructor
  Right (preds, program)

compile0toRuleDefinition :: Ast0.Ast -> CompileResult RuleDefinition
compile0toRuleDefinition (Ast0.Symbol _) = Left InvalidRuleDefinition
compile0toRuleDefinition (Ast0.Compound xs) =
  -- rules must have exactly 3 subterms:
  --  1   2 3
  -- (def a b)
  if length xs /= 3
    then Left InvalidRuleDefinition
    else
      let startsWithDefSymbol :: [Ast0.Ast] -> Bool
          startsWithDefSymbol (Ast0.Symbol "def" : _) = True
          startsWithDefSymbol _ = False

          ruleDefinitionPattern :: [Ast0.Ast] -> Ast0.Ast
          ruleDefinitionPattern ast = ast !! 1

          ruleDefinitionConstructor :: [Ast0.Ast] -> Ast0.Ast
          ruleDefinitionConstructor ast = ast !! 2
       in if startsWithDefSymbol xs
            then
              let pat = compile0to1 $ xs !! 1
                  constr = xs !! 2
               in do
                    pat' <- compile1toP0 pat
                    vars <- p0VariableBindings pat'
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
-- ruleDefinitionVariableBindings :: RuleDefinition -> CompileResult Variables
-- ruleDefinitionVariableBindings (RuleDefinition pat _constructor) =
--   cata go (indexP0ByC0 pat)
--   where
--     go ::
--       CofreeF
--         AstP0.AstF
--         AstC0.Index
--         (CompileResult Variables) ->
--       CompileResult Variables
--     go (index :< ast) = case ast of
--       AstP0.SymbolF s ->
--         Right $
--           if s `elem` vars
--             then H.singleton s index
--             else H.empty
--       AstP0.CompoundWithoutEllipsesF xs -> do
--         xs' <- sequence xs
--         let combined = unionNonIntersectingHashMaps xs'
--         maybeToEither VariableUsedMoreThanOnceInPattern combined
--       AstP0.CompoundWithEllipsesF b e a -> do
--         b' <- sequence b
--         e' <- e
--         a' <- sequence a
--         let combined = unionNonIntersectingHashMaps $ e' : (b' ++ a')
--         maybeToEither VariableUsedMoreThanOnceInPattern combined

-- Returns a list of conditions that must hold for a given rule's pattern to
-- match a term.
--
-- For example, in the following rule:
--
--   (def xs (flatten (list (list xs ..) ..)) -> (list xs .. ..))
-- Data.HashMap
-- the following conditions must hold if the rule is to match a given term:
--
-- - Index [] is a compound term of length == 2
-- - Index [0] == "flatten"
-- - Index [1] is a compound term of length >= 1
-- - Index [1,0] == "list"
-- - Indices [1,1..length] are compound terms of length >= 1
-- - Indices [1,1..length,0] == "list"
ruleDefinitionPredicates :: RuleDefinition -> CompileResult [IndexedPredicate]
ruleDefinitionPredicates (RuleDefinition vars pat _constructor) = cata go (indexP0ByC0 pat)
  where
    go ::
      Cata (Cofree AstP0.AstF AstC0.Index) (CompileResult [IndexedPredicate])
    go (index :< ast) = case ast of
      AstP0.SymbolF s ->
        Right [IndexedPredicate (SymbolEqualTo s) index | not $ H.member s vars]
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

compile1toC0 :: VariableBindings -> Ast1.Ast -> AstC0.Ast
compile1toC0 vars = cata $ \case
  Ast1.SymbolF s -> case H.lookup s vars of
    Nothing -> AstC0.Symbol s
    Just index -> AstC0.Variable index
  Ast1.CompoundF xs -> AstC0.Compound xs
  Ast1.EllipsesF x -> AstC0.Ellipses x

data C0ToC1Data = C0ToC1Data
  { _ast :: !AstC1P.Ast,
    _nextUnusedVar :: !Var,
    _remainingAssignment :: Maybe (Var, AstC0.Index, Between)
  }
  deriving (Show)

compileC0ToC1P :: AstC0.Ast -> CompileResult (AstC1P.Ast, Var)
compileC0ToC1P ast = do
  d <- cata traverseC0ToC1P ast firstUnusedVar
  case _remainingAssignment d of
    Just _ -> Left TooFewEllipsesInConstructor
    Nothing ->
      Right (_ast d, _nextUnusedVar d)
  where
    firstUnusedVar :: Var
    firstUnusedVar = 0

traverseC0ToC1P :: Cata AstC0.Ast (Var -> CompileResult C0ToC1Data)
traverseC0ToC1P a nextUnusedVar = case a of
  AstC0.SymbolF s ->
    Right $
      C0ToC1Data
        { _ast = AstC1P.Symbol s,
          _nextUnusedVar = nextUnusedVar,
          _remainingAssignment = Nothing
        }
  AstC0.VariableF i ->
    let (c0, c1) = popTrailingC1Index i
        copyAst = AstC1P.Copy nextUnusedVar
     in Right $
          C0ToC1Data
            { _ast =
                if null c1
                  then copyAst
                  else
                    let location = if null c0 then TopLevel else NotTopLevel
                     in AstC1P.Assignment (nextUnusedVar, c1, location) copyAst,
              _nextUnusedVar = nextUnusedVar + 1,
              _remainingAssignment =
                if null c0
                  then Nothing
                  else Just $
                    case popBetweenTail c0 of
                      (c0', Just (zeroPlus, lenMinus)) ->
                        (nextUnusedVar, c0', Between zeroPlus lenMinus)
                      _ -> error "unreachable"
            }
  AstC0.EllipsesF x -> do
    C0ToC1Data ast nextUnusedVar remainingAssignment <- x nextUnusedVar
    case remainingAssignment of
      Nothing -> Left TooManyEllipsesInConstructor
      Just (var, c0, Between zeroPlus lenMinus) ->
        let (c0', c1) = popTrailingC1Index c0
            loopAst =
              AstC1P.Loop
                { AstC1P.var = var,
                  AstC1P.src = nextUnusedVar + 1,
                  AstC1P.start = zeroPlus,
                  AstC1P.end = lenMinus,
                  AstC1P.body = ast
                }
         in Right $
              C0ToC1Data
                { _ast =
                    if null c1
                      then
                        if null c0'
                          then AstC1P.Assignment (nextUnusedVar + 1, c1, TopLevel) loopAst
                          else loopAst
                      else
                        let location = if null c0' then TopLevel else NotTopLevel
                         in AstC1P.Assignment (nextUnusedVar + 1, c1, location) loopAst,
                  _nextUnusedVar = nextUnusedVar + 2,
                  _remainingAssignment =
                    if null c0'
                      then Nothing
                      else Just $
                        case popBetweenTail c0' of
                          (c0', Just (zeroPlus, lenMinus)) ->
                            (nextUnusedVar + 1, c0', Between zeroPlus lenMinus)
                          _ -> error "unreachable"
                }
  AstC0.CompoundF xs -> cata mergeXS xs nextUnusedVar
    where
      mergeXS :: Cata [Var -> CompileResult C0ToC1Data] (Var -> CompileResult C0ToC1Data)
      mergeXS Nil nextUnusedVar =
        Right $
          C0ToC1Data
            { _ast = AstC1P.Compound [],
              _nextUnusedVar = nextUnusedVar,
              _remainingAssignment = Nothing
            }
      mergeXS (Cons x xs) nextUnusedVar = do
        C0ToC1Data astX nextUnusedVar remainingAssignmentX <- x nextUnusedVar
        C0ToC1Data ast nextUnusedVar remainingAssignment <- xs nextUnusedVar
        remainingAssignment <- compatibleRemainingAssignment remainingAssignmentX remainingAssignment
        let compoundInternals =
              case ast of
                AstC1P.Compound compoundInternals -> compoundInternals
                _ -> error "unreachable"
        let ast = AstC1P.Compound $ astX : compoundInternals
        pure $
          C0ToC1Data
            { _ast = ast,
              _nextUnusedVar = nextUnusedVar,
              _remainingAssignment = remainingAssignment
            }
        where
          compatibleRemainingAssignment ::
            Maybe (Var, AstC0.Index, Between) ->
            Maybe (Var, AstC0.Index, Between) ->
            CompileResult (Maybe (Var, AstC0.Index, Between))
          compatibleRemainingAssignment Nothing Nothing = Right Nothing
          compatibleRemainingAssignment (Just t) Nothing = Right $ Just t
          compatibleRemainingAssignment Nothing (Just t) = Right $ Just t
          compatibleRemainingAssignment (Just t) (Just u) =
            if t == u
              then Right $ Just u
              else Left VarsNotCapturedUnderSameEllipsisInConstructor

data C1ToC2InputData = C1ToC2InputData
  { _c2iNextUnusedVar :: Var,
    _c2iCompoundTermLengthCounter :: Maybe Var
  }

incC2Var :: C1ToC2InputData -> C1ToC2InputData
incC2Var d =
  let v = _c2iNextUnusedVar d
   in d {_c2iNextUnusedVar = v + 1}

setLengthCountVar :: Var -> C1ToC2InputData -> C1ToC2InputData
setLengthCountVar v d = d {_c2iCompoundTermLengthCounter = Just v}

newLengthCountVar :: State C1ToC2InputData Var
newLengthCountVar = do
  var <- newVar
  modify $ setLengthCountVar var
  pure var

newVar :: State C1ToC2InputData Var
newVar = do
  var <- gets _c2iNextUnusedVar
  modify incC2Var
  pure var

indexAssignStmts :: Var -> AssignmentLocation -> AstC1P.Index -> AstC2.Ast NamedLabel
indexAssignStmts var loc = addAssignmentToInputWhenToplevel . cata go
  where
    go :: Cata AstC1P.Index (AstC2.Ast NamedLabel)
    go = \case
      Nil -> []
      Cons i stmts -> assignment : stmts
        where
          assignment =
            AstC2.Assign
              AstC2Assign.Assign
                { AstC2Assign.lhs = var,
                  AstC2Assign.rhs =
                    C2Expr.BinOp
                      C2Expr.ArrayAccess
                      (C2Expr.Var var)
                      ( case i of
                          AstC1P.ZeroPlus zeroPlus ->
                            C2Expr.Nat zeroPlus
                          AstC1P.LenMinus lenMinus ->
                            C2Expr.BinOp
                              C2Expr.Sub
                              (C2Expr.Length $ C2Expr.Var var)
                              (C2Expr.Nat lenMinus)
                      )
                }
    addAssignmentToInputWhenToplevel :: AstC2.Ast NamedLabel -> AstC2.Ast NamedLabel
    addAssignmentToInputWhenToplevel stmts = case loc of
      NotTopLevel -> stmts
      TopLevel -> s : stmts
        where
          s =
            AstC2.Assign
              AstC2Assign.Assign
                { AstC2Assign.lhs = var,
                  AstC2Assign.rhs = C2Expr.Input
                }

compileC1PToC2 :: Var -> AstC1P.Ast -> AstC2.Ast NamedLabel
compileC1PToC2 nextUnusedVar ast = evalState (para go ast) initialState
  where
    initialState :: C1ToC2InputData
    initialState =
      C1ToC2InputData
        { _c2iNextUnusedVar = nextUnusedVar,
          _c2iCompoundTermLengthCounter = Nothing
        }
    isC1PNonLoopVariant :: AstC1P.Ast -> Bool
    isC1PNonLoopVariant (AstC1P.Loop {}) = False
    isC1PNonLoopVariant (AstC1P.Assignment _ x) = isC1PNonLoopVariant x
    isC1PNonLoopVariant _ = True

    -- We use 'para' instead of 'cata' because in the 'CompoundF' case, we
    -- need to be able to count the number of non-loops in the subterms.
    go :: Para AstC1P.Ast (State C1ToC2InputData (AstC2.Ast NamedLabel))
    go = \case
      AstC1P.SymbolF s -> do
        pure [AstC2.Push $ C2Expr.Symbol s]
      AstC1P.CompoundF inputXsPairs -> do
        lengthCountVar <- newLengthCountVar
        xs <- concat <$> mapM snd inputXsPairs
        let inputs = map fst inputXsPairs
            numNonLoopInputs = length . filter isC1PNonLoopVariant $ inputs
            initLengthCountVar =
              AstC2.Assign $
                AstC2Assign.Assign
                  { AstC2Assign.lhs = lengthCountVar,
                    AstC2Assign.rhs =
                      C2Expr.Nat numNonLoopInputs
                  }
            buildCompoundTerm =
              AstC2.Build $ C2Expr.Var lengthCountVar
        pure $ initLengthCountVar : xs ++ [buildCompoundTerm]
      AstC1P.AssignmentF (var, index, loc) inputXPair -> do
        x <- snd inputXPair
        let assignmentStmts = indexAssignStmts var loc index
        pure $ assignmentStmts ++ x
      AstC1P.CopyF v -> do
        pure [AstC2.Push $ C2Expr.Var v]
      AstC1P.LoopF var src start end inputXPair -> do
        --        #0 = start              ; #0 is 'loopCounterVar'
        --        #1 = #src.length - end  ; #1 is 'loopEndVar'
        --        jump BOT
        -- TOP:   #var = #src[#0]
        --        x ...
        --        #0 = #0 + 1
        --        #lc = #lc + 1           ; #lc is 'lengthCountVar'
        -- BOT:   jump TOP if #0 < #1
        maybeLengthCountVar <- gets _c2iCompoundTermLengthCounter
        let lengthCountVar = case maybeLengthCountVar of
              Nothing -> error "unreachable"
              Just lengthCountVar -> lengthCountVar
        loopCounterVar <- newVar
        loopEndVar <- newVar
        loopLabel <- newVar
        x <- snd inputXPair
        let assignLoopCountVar =
              AstC2.Assign
                AstC2Assign.Assign
                  { AstC2Assign.lhs = loopCounterVar,
                    AstC2Assign.rhs = C2Expr.Nat start
                  }
            assignLoopEndVar =
              AstC2.Assign
                AstC2Assign.Assign
                  { AstC2Assign.lhs = loopEndVar,
                    AstC2Assign.rhs =
                      C2Expr.BinOp
                        C2Expr.Sub
                        (C2Expr.Length $ C2Expr.Var src)
                        (C2Expr.Nat end)
                  }
            jumpBot =
              AstC2.Jump
                AstC2Jump.Jump
                  { AstC2Jump.target = BotOfLoop loopLabel,
                    AstC2Jump.condition = AstC2Expr.Bool True
                  }
            assignVarToSrc =
              AstC2.Assign
                AstC2Assign.Assign
                  { AstC2Assign.lhs = var,
                    AstC2Assign.rhs =
                      C2Expr.BinOp
                        C2Expr.ArrayAccess
                        (C2Expr.Var src)
                        (C2Expr.Var loopCounterVar)
                  }
            incremeentLoopCountVar =
              AstC2.Assign
                AstC2Assign.Assign
                  { AstC2Assign.lhs = loopCounterVar,
                    AstC2Assign.rhs =
                      C2Expr.BinOp
                        C2Expr.Add
                        (C2Expr.Var loopCounterVar)
                        (C2Expr.Nat 1)
                  }
            incremenetLengthCountVar =
              AstC2.Assign
                AstC2Assign.Assign
                  { AstC2Assign.lhs = lengthCountVar,
                    AstC2Assign.rhs =
                      C2Expr.BinOp
                        C2Expr.Add
                        (C2Expr.Var lengthCountVar)
                        (C2Expr.Nat 1)
                  }
            jumpTop =
              AstC2.Jump
                AstC2Jump.Jump
                  { AstC2Jump.target = TopOfLoop loopLabel,
                    AstC2Jump.condition =
                      C2Expr.BinOp
                        C2Expr.LessThan
                        (C2Expr.Var loopCounterVar)
                        (C2Expr.Var loopEndVar)
                  }
        pure $
          [ assignLoopCountVar,
            assignLoopEndVar,
            jumpBot,
            assignVarToSrc
          ]
            ++ x
            ++ [ incremeentLoopCountVar,
                 incremenetLengthCountVar,
                 jumpTop
               ]

data NamedLabel = TopOfLoop !Int | BotOfLoop !Int deriving (Eq, Generic)

instance Hashable NamedLabel

resolveC2NamedLabels :: AstC2.Ast NamedLabel -> AstC2.Ast Int
resolveC2NamedLabels ast = replaceNamesWithOffsets namedLabelOffsets ast
  where
    namedLabelOffsets :: H.HashMap NamedLabel Int
    namedLabelOffsets = H.fromList $ cata go offsetStmtPairs
      where
        offsetStmtPairs :: [(Int, AstC2.Stmt NamedLabel)]
        offsetStmtPairs = zip [0 ..] ast

        go :: Cata [(Int, AstC2.Stmt NamedLabel)] [(NamedLabel, Int)]
        go = \case
          Nil -> []
          Cons (offset, stmt) labelOffsetPairs -> case stmt of
            -- Jump statements are the only ones which contain labels,
            -- so they are the only ones we care about here.
            AstC2.Jump (AstC2Jump.Jump label _condition) ->
              (label, offset) : labelOffsetPairs
            _stmtNotContainingLabels -> labelOffsetPairs

    replaceNamesWithOffsets ::
      H.HashMap NamedLabel Int ->
      AstC2.Ast NamedLabel ->
      AstC2.Ast Int
    replaceNamesWithOffsets ht = cata go
      where
        go :: Cata [AstC2.Stmt NamedLabel] [AstC2.Stmt Int]
        go = \case
          Nil -> []
          Cons namedStmt offsetStmts -> offsetStmt : offsetStmts
            where
              offsetStmt :: AstC2.Stmt Int
              offsetStmt = replaceNameWithOffset <$> namedStmt

              replaceNameWithOffset :: NamedLabel -> Int
              replaceNameWithOffset = \case
                -- A loop is structured like this:
                --
                --      jump BOT
                -- TOP: <first loop body statement>
                --      <rest of loop body statements> ...
                -- BOT: jump TOP if <condition>
                --
                -- So, to find the line number of 'TOP', add 1 to
                -- line number of the 'jump BOT' satement.
                TopOfLoop name -> 1 + fromJust (ht !? BotOfLoop name)
                BotOfLoop name -> fromJust $ ht !? TopOfLoop name