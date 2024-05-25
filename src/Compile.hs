{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Compile
  ( compile,
    compile0toRuleDefinition,
    ruleDefinitionPredicates,
    compile0to1,
    compile1toP0,
    compile1toC0,
    VariableBindings,
    RuleDefinition (..),
    compileRule2,
    compileC0ToC1P,
    C0ToC1Data (..),
    findOverlappingPatterns,
    errOnOverlappingPatterns,
  )
where

import Ast0 qualified
import Ast1 qualified
import AstC0 qualified
import AstC1 (AssignmentLocation (..))
import AstC1 qualified
import AstC2 qualified
import AstC2Assign qualified
import AstC2Expr qualified
import AstC2Expr qualified as C2Expr
import AstC2Jump qualified
import AstP0 (indexP0ByC0)
import AstP0 qualified
import Control.Comonad (Comonad (..))
import Control.Comonad qualified as C
import Control.Comonad.Cofree (Cofree, hoistCofree)
import Control.Comonad.Cofree qualified as C
import Control.Comonad.Identity (Identity (Identity))
import Control.Comonad.Trans.Cofree (CofreeF (..))
import Control.Monad.State.Strict
  ( State,
    evalState,
    gets,
    modify,
    withState,
  )
import Data.Bifunctor qualified
import Data.Either.Extra (maybeToEither)
import Data.Functor.Foldable (Corecursive (..), ListF (..), Recursive (..))
import Data.HashMap.Strict ((!?))
import Data.HashMap.Strict qualified as H
import Data.Hashable (Hashable)
import Data.Maybe (fromJust)
import Error (CompileResult, ErrorType (..), addLength, badEllipsesCountErrorMessage, genericErrorInfo)
import GHC.Generics (Generic)
import Predicate (IndexedPredicate (..), Predicate (LengthEqualTo, LengthGreaterThanOrEqualTo, SymbolEqualTo), applyPredicates)
import Read (SrcLocked, SrcLockedF)
import Utils (Between (..), Cata, Para, Span (Span), popBetweenTail, popTrailingC1Index)
import Var (Var)

type VariableBindings = H.HashMap String AstC0.Index

compile :: VariableBindings -> SrcLocked Ast0.Ast -> CompileResult (SrcLocked (AstC2.Ast Int))
compile vars ast = do
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

compile1toP0 :: SrcLocked Ast1.Ast -> CompileResult (SrcLocked AstP0.Ast)
compile1toP0 = para go
  where
    go :: Para (SrcLocked Ast1.Ast) (CompileResult (SrcLocked AstP0.Ast))
    go = \case
      l :< Ast1.SymbolF s -> Right $ l C.:< AstP0.SymbolF s
      l :< Ast1.CompoundF inputXsPairs ->
        let input :: [SrcLocked Ast1.Ast]
            input = map fst inputXsPairs
            xs :: [CompileResult (SrcLocked AstP0.Ast)]
            xs = map snd inputXsPairs
            wasEllipses :: (SrcLocked Ast1.Ast, SrcLocked AstP0.Ast) -> Bool
            wasEllipses = \case
              (_ C.:< Ast1.EllipsesF _, _) -> True
              _ -> False
         in do
              inputXsPairs <- zip input <$> sequence xs
              let inputXsPairsSplit = splitBeforeAndAfter wasEllipses inputXsPairs
              case inputXsPairsSplit of
                Nothing ->
                  Right $ l C.:< AstP0.CompoundWithoutEllipsesF (map snd inputXsPairs)
                Just (b, e, a) ->
                  if any wasEllipses a
                    then Left $ genericErrorInfo MoreThanOneEllipsisInSingleCompoundTermOfPattern
                    else Right $ l C.:< AstP0.CompoundWithEllipsesF (map snd b) (snd e) (map snd a)
      l :< Ast1.EllipsesF x -> extract x

data RuleDefinition = RuleDefinition
  { _variables :: !VariableBindings,
    _pattern :: !(SrcLocked AstP0.Ast),
    _constructor :: !(SrcLocked Ast0.Ast)
  }

isDollarSignVar :: String -> Bool
isDollarSignVar ('$' : _) = True
isDollarSignVar _ = False

p0VariableBindings :: SrcLocked AstP0.Ast -> CompileResult VariableBindings
p0VariableBindings = cata go . indexP0ByC0
  where
    go :: Cata (Cofree AstP0.AstF (Span Int, AstC0.Index)) (CompileResult VariableBindings)
    go ((_l, index) :< ast) = case ast of
      AstP0.SymbolF s ->
        Right $
          if isDollarSignVar s
            then H.singleton s index
            else H.empty
      AstP0.CompoundWithoutEllipsesF xs -> do
        xs' <- sequence xs
        let combined = unionNonIntersectingHashMaps xs'
        maybeToEither (genericErrorInfo VariableUsedMoreThanOnceInPattern) combined
      AstP0.CompoundWithEllipsesF b e a -> do
        b' <- sequence b
        e' <- e
        a' <- sequence a
        let combined = unionNonIntersectingHashMaps $ e' : (b' ++ a')
        maybeToEither (genericErrorInfo VariableUsedMoreThanOnceInPattern) combined

compileRule2 :: RuleDefinition -> CompileResult (([IndexedPredicate], SrcLocked AstP0.Ast), SrcLocked (AstC2.Ast Int))
compileRule2 rule@(RuleDefinition vars pattern constructor) = do
  preds <- ruleDefinitionPredicates rule
  program <- compile vars constructor
  Right ((preds, pattern), program)

errOnOverlappingPatterns :: [([IndexedPredicate], SrcLocked AstP0.Ast)] -> CompileResult ()
errOnOverlappingPatterns predicatesPatternPairs =
  case findOverlappingPatterns predicatesPatternPairs of
    Nothing -> Right ()
    Just pair -> Left (genericErrorInfo (OverlappingPatterns {- pair -}))

findOverlappingPatterns :: [([IndexedPredicate], SrcLocked AstP0.Ast)] -> Maybe (SrcLocked AstP0.Ast, SrcLocked AstP0.Ast)
findOverlappingPatterns predicatesPatternPairs = Nothing -- TODO!
-- let removedEllipses =
--       zipWith
--         ( \i (preds, p0Ast) ->
--             (i, preds, p0Ast, removeEllipses p0Ast)
--         )
--         [0 ..]
--         predicatesPatternPairs
--     go ::
--       Cata
--         [ ( (Int, [IndexedPredicate], AstP0.Ast, Ast0.Ast),
--             (Int, [IndexedPredicate], AstP0.Ast, Ast0.Ast)
--           )
--         ]
--         (Maybe (AstP0.Ast, AstP0.Ast))
--     go = \case
--       Nil -> Nothing
--       Cons ((i, preds, astP0, _ast0), (i', _preds', astP0', ast0')) answer ->
--         case answer of
--           Just answer -> Just answer
--           Nothing ->
--             if i == i'
--               then Nothing
--               else
--                 if applyPredicates preds ast0'
--                   then Just (astP0, astP0')
--                   else Nothing
--     pairs = [(a, b) | a <- removedEllipses, b <- removedEllipses]
--  in cata go pairs

removeEllipses :: AstP0.Ast -> Ast0.Ast
removeEllipses = cata go
  where
    go :: Cata AstP0.Ast Ast0.Ast
    go = \case
      AstP0.SymbolF s -> Ast0.Symbol s
      AstP0.CompoundWithoutEllipsesF xs -> Ast0.Compound xs
      AstP0.CompoundWithEllipsesF b e a -> Ast0.Compound $ b ++ [e] ++ a

-- predicateListsOverlap :: [IndexedPredicate] -> [IndexedPredicate] -> Bool
-- predicateListsOverlap preds1 preds2 = _

compile0toRuleDefinition :: SrcLocked Ast0.Ast -> CompileResult RuleDefinition
compile0toRuleDefinition (_ C.:< Ast0.SymbolF _) = Left (genericErrorInfo InvalidRuleDefinition)
compile0toRuleDefinition (_ C.:< Ast0.CompoundF xs) =
  -- rules must have exactly 3 subterms:
  --  1   2 3
  -- (def a b)
  if length xs /= 3
    then Left (genericErrorInfo InvalidRuleDefinition)
    else
      let xs' :: [SrcLocked Ast0.Ast]
          xs' = xs
          startsWithDefSymbol :: [SrcLocked Ast0.Ast] -> Bool
          startsWithDefSymbol ((_ C.:< Ast0.SymbolF "def") : _) = True
          startsWithDefSymbol _ = False
       in if startsWithDefSymbol xs'
            then
              let pat = compile0to1 $ xs' !! 1
                  constr = xs' !! 2
               in do
                    pat' <- compile1toP0 pat
                    vars <- p0VariableBindings pat'
                    Right $ RuleDefinition vars pat' constr
            else Left (genericErrorInfo InvalidRuleDefinition)

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
      Cata (Cofree AstP0.AstF (Span Int, AstC0.Index)) (CompileResult [IndexedPredicate])
    go ((_l, index) :< ast) = case ast of
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

compile0to1 :: SrcLocked Ast0.Ast -> SrcLocked Ast1.Ast
compile0to1 = cata $ \case
  l :< Ast0.SymbolF s -> l C.:< Ast1.SymbolF s
  l :< Ast0.CompoundF xs -> l C.:< Ast1.CompoundF (go xs)
    where
      go :: [SrcLocked Ast1.Ast] -> [SrcLocked Ast1.Ast]
      go ((l1 C.:< y) : (l2 C.:< Ast1.SymbolF "..") : ys) = go (y' : ys)
        where
          y' :: SrcLocked Ast1.Ast
          y' = addLength l1 l2 C.:< Ast1.EllipsesF (l1 C.:< y)
      go (y : ys) = y : go ys
      go [] = []

compile1toC0 :: VariableBindings -> SrcLocked Ast1.Ast -> SrcLocked AstC0.Ast
compile1toC0 vars = cata $ \case
  l :< Ast1.SymbolF s -> case H.lookup s vars of
    Nothing -> l C.:< AstC0.SymbolF s
    Just index -> l C.:< AstC0.VariableF index
  l :< Ast1.CompoundF xs -> l C.:< AstC0.CompoundF xs
  l :< Ast1.EllipsesF x -> l C.:< AstC0.EllipsesF x

data C0ToC1Data = C0ToC1Data
  { _ast :: !(SrcLocked AstC1.Ast),
    _nextUnusedVar :: !Var,
    _remainingAssignment :: Maybe (Var, AstC0.Index, Between)
  }

compileC0ToC1P :: SrcLocked AstC0.Ast -> CompileResult (SrcLocked AstC1.Ast, Var)
compileC0ToC1P ast = do
  d <- cata traverseC0ToC1P ast firstUnusedVar
  case _remainingAssignment d of
    Just _ -> Left (badEllipsesCountErrorMessage 1 0 span1 span2)
      where
        span1 = Span 16 2
        span2 = Span 30 2
    Nothing ->
      Right (_ast d, _nextUnusedVar d)
  where
    firstUnusedVar :: Var
    firstUnusedVar = 0

traverseC0ToC1P :: Cata (SrcLocked AstC0.Ast) (Var -> CompileResult C0ToC1Data)
traverseC0ToC1P a nextUnusedVar = case a of
  l :< AstC0.SymbolF s ->
    Right $
      C0ToC1Data
        { _ast = l C.:< AstC1.SymbolF s,
          _nextUnusedVar = nextUnusedVar,
          _remainingAssignment = Nothing
        }
  l :< AstC0.VariableF i ->
    let (c0, c1) = popTrailingC1Index i
        copyAst = l C.:< AstC1.CopyF nextUnusedVar
     in Right $
          C0ToC1Data
            { _ast =
                if null c1
                  then copyAst
                  else
                    let location = if null c0 then TopLevel else NotTopLevel
                     in l C.:< AstC1.AssignmentF (nextUnusedVar, c1, location) copyAst,
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
  l :< AstC0.EllipsesF x -> do
    C0ToC1Data ast nextUnusedVar remainingAssignment <- x nextUnusedVar
    case remainingAssignment of
      Nothing -> Left (genericErrorInfo BadEllipsesCount {- too many -})
      Just (var, c0, Between zeroPlus lenMinus) ->
        let (c0', c1) = popTrailingC1Index c0
            loopAst =
              l
                C.:< AstC1.LoopF
                  { AstC1.varF = var,
                    AstC1.srcF = nextUnusedVar + 1,
                    AstC1.startF = zeroPlus,
                    AstC1.endF = lenMinus,
                    AstC1.bodyF = ast
                  }
         in Right $
              C0ToC1Data
                { _ast =
                    if null c1
                      then
                        if null c0'
                          then l C.:< AstC1.AssignmentF (nextUnusedVar + 1, c1, TopLevel) loopAst
                          else loopAst
                      else
                        let location = if null c0' then TopLevel else NotTopLevel
                         in l C.:< AstC1.AssignmentF (nextUnusedVar + 1, c1, location) loopAst,
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
  l :< AstC0.CompoundF xs -> cata mergeXS xs nextUnusedVar
    where
      mergeXS :: Cata [Var -> CompileResult C0ToC1Data] (Var -> CompileResult C0ToC1Data)
      mergeXS Nil nextUnusedVar =
        Right $
          C0ToC1Data
            { _ast = l C.:< AstC1.CompoundF [],
              _nextUnusedVar = nextUnusedVar,
              _remainingAssignment = Nothing
            }
      mergeXS (Cons x xs) nextUnusedVar = do
        C0ToC1Data astX nextUnusedVar remainingAssignmentX <- x nextUnusedVar
        C0ToC1Data ast nextUnusedVar remainingAssignment <- xs nextUnusedVar
        remainingAssignment <- compatibleRemainingAssignment remainingAssignmentX remainingAssignment
        let compoundInternals =
              case ast of
                _l C.:< AstC1.CompoundF compoundInternals -> compoundInternals
                _ -> error "unreachable"
        let ast = l C.:< AstC1.CompoundF (astX : compoundInternals)
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
              else Left (genericErrorInfo VarsNotCapturedUnderSameEllipsisInConstructor)

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

indexAssignStmts :: Span Int -> Var -> AssignmentLocation -> AstC1.Index -> SrcLocked (AstC2.Ast NamedLabel)
indexAssignStmts l var loc = mapSrcLock l . addAssignmentToInputWhenToplevel . cata go
  where
    go :: Cata AstC1.Index (AstC2.Ast NamedLabel)
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
                          AstC1.ZeroPlus zeroPlus ->
                            C2Expr.Nat zeroPlus
                          AstC1.LenMinus lenMinus ->
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

mapSrcLock :: Span Int -> [a] -> Cofree (ListF a) (Span Int)
mapSrcLock s = cata (s C.:<)

cofreeAppend :: Cofree (ListF a) b -> Cofree (ListF a) b -> Cofree (ListF a) b
cofreeAppend (_ C.:< Nil) cf = cf
cofreeAppend (l1 C.:< Cons x xs) cf = l1 C.:< Cons x (cofreeAppend xs cf)

cofreeConcat2 :: Cofree (ListF (Cofree (ListF a) b)) b -> Cofree (ListF a) b
cofreeConcat2 = cata go
  where
    go :: Cata (Cofree (ListF (Cofree (ListF a) b)) b) (Cofree (ListF a) b)
    go = \case
      l :< Nil -> l C.:< Nil
      _ :< Cons x xs -> cofreeAppend x xs

cofreeConcat :: b -> [Cofree (ListF a) b] -> Cofree (ListF a) b
cofreeConcat l = foldr cofreeAppend (l C.:< Nil)

compileC1PToC2 :: Var -> SrcLocked AstC1.Ast -> SrcLocked (AstC2.Ast NamedLabel)
compileC1PToC2 nextUnusedVar ast = evalState (para go ast) initialState
  where
    initialState :: C1ToC2InputData
    initialState =
      C1ToC2InputData
        { _c2iNextUnusedVar = nextUnusedVar,
          _c2iCompoundTermLengthCounter = Nothing
        }
    isC1PNonLoopVariant :: SrcLocked AstC1.Ast -> Bool
    isC1PNonLoopVariant (_ C.:< AstC1.LoopF {}) = False
    isC1PNonLoopVariant (_ C.:< AstC1.AssignmentF _ x) = isC1PNonLoopVariant x
    isC1PNonLoopVariant _ = True

    -- We use 'para' instead of 'cata' because in the 'CompoundF' case, we
    -- need to be able to count the number of non-loops in the subterms.
    go :: Para (SrcLocked AstC1.Ast) (State C1ToC2InputData (SrcLocked (AstC2.Ast NamedLabel)))
    go = \case
      l :< AstC1.SymbolF s -> do
        pure $ mapSrcLock l [AstC2.Push $ C2Expr.Symbol s]
      l :< AstC1.CompoundF inputXsPairs -> do
        lengthCountVar <- newLengthCountVar
        let resetLengthCountVarInX ::
              (SrcLocked AstC1.Ast, State C1ToC2InputData (SrcLocked [AstC2.Stmt NamedLabel])) ->
              (SrcLocked AstC1.Ast, State C1ToC2InputData (SrcLocked [AstC2.Stmt NamedLabel]))
            resetLengthCountVarInX = Data.Bifunctor.second $ withState $ setLengthCountVar lengthCountVar
        let inputXsPairs' :: [(SrcLocked AstC1.Ast, State C1ToC2InputData (SrcLocked [AstC2.Stmt NamedLabel]))]
            inputXsPairs' = map resetLengthCountVarInX inputXsPairs
        let q1 :: State C1ToC2InputData [SrcLocked [AstC2.Stmt NamedLabel]]
            q1 = mapM snd inputXsPairs'

            q2 :: State C1ToC2InputData (SrcLocked [AstC2.Stmt NamedLabel])
            q2 = cofreeConcat l <$> q1
        xs <- q2
        let inputs :: [SrcLocked AstC1.Ast]
            inputs = map fst inputXsPairs'
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
        pure
          ( mapSrcLock l [initLengthCountVar]
              `cofreeAppend` xs
              `cofreeAppend` mapSrcLock l [buildCompoundTerm]
          )
      l :< AstC1.AssignmentF (var, index, loc) inputXPair -> do
        x <- snd inputXPair
        let assignmentStmts = indexAssignStmts l var loc index
        pure $ assignmentStmts `cofreeAppend` x
      l :< AstC1.CopyF v -> do
        pure $ mapSrcLock l [AstC2.Push $ C2Expr.Var v]
      l :< AstC1.LoopF var src start end inputXPair -> do
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
            srcLockedPrologue =
              mapSrcLock
                l
                [ assignLoopCountVar,
                  assignLoopEndVar,
                  jumpBot,
                  assignVarToSrc
                ]
            srcLockedEpilogue =
              mapSrcLock
                l
                [ incremeentLoopCountVar,
                  incremenetLengthCountVar,
                  jumpTop
                ]
        pure $ srcLockedPrologue `cofreeAppend` x `cofreeAppend` srcLockedEpilogue

data NamedLabel = TopOfLoop !Int | BotOfLoop !Int deriving (Eq, Generic)

instance Hashable NamedLabel

enumerateCofree :: Cofree (ListF a) b -> Cofree (ListF a) (Int, b)
enumerateCofree = cata $ \case
  b :< Nil -> (0, b) C.:< Nil
  b :< Cons x xs -> (i + 1, b) C.:< Cons x xs
    where
      ((i, _) C.:< _) = xs

resolveC2NamedLabels :: SrcLocked (AstC2.Ast NamedLabel) -> SrcLocked (AstC2.Ast Int)
resolveC2NamedLabels ast = replaceNamesWithOffsets namedLabelOffsets ast
  where
    namedLabelOffsets :: H.HashMap NamedLabel Int
    namedLabelOffsets = H.fromList $ cata go offsetStmtPairs
      where
        offsetStmtPairs :: Cofree (ListF (AstC2.Stmt NamedLabel)) (Int, Span Int)
        offsetStmtPairs = enumerateCofree ast

        go :: Cata (Cofree (ListF (AstC2.Stmt NamedLabel)) (Int, Span Int)) [(NamedLabel, Int)]
        go = \case
          (_offset, _l) :< Nil -> []
          (offset, _l) :< Cons stmt labelOffsetPairs -> case stmt of
            -- Jump statements are the only ones which contain labels,
            -- so they are the only ones we care about here.
            AstC2.Jump (AstC2Jump.Jump label _condition) ->
              (label, offset) : labelOffsetPairs
            _stmtNotContainingLabels -> labelOffsetPairs

    replaceNamesWithOffsets ::
      H.HashMap NamedLabel Int ->
      SrcLocked (AstC2.Ast NamedLabel) ->
      SrcLocked (AstC2.Ast Int)
    replaceNamesWithOffsets ht = cata go
      where
        go :: Cata (SrcLocked (AstC2.Ast NamedLabel)) (SrcLocked (AstC2.Ast Int))
        go = \case
          l :< Nil -> l C.:< Nil
          l :< Cons namedStmt offsetStmts -> l C.:< Cons offsetStmt offsetStmts
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