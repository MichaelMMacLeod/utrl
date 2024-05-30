{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Compile
  ( compile0to1,
    compile1toP0,
    compile1toC0,
    compileC0ToC1,
    errorsToEither,
    requestConstructorC2,
    requestPredicates,
  )
where

import Analyze
  ( analyzeDefinitionSyntax,
    analyzeEllipsesAppliedToSymbols,
    analyzeEllipsesCaptures,
    analyzeEllipsesCapturesWithoutVariables,
    analyzePatternForMoreThan1EllipsisPerTerm,
    analyzeVariableNotMatchedInPattern,
    analyzeVariablesUsedMoreThanOnceInPattern,
    ruleDefinitionPredicates,
    unreachableBecauseOfAnalysisStep,
  )
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
import CompileTypes
  ( CompileRequest,
    DefinitionStorage (..),
    VariableBindings,
  )
import Control.Comonad (Comonad (..))
import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Cofree qualified as C
import Control.Comonad.Trans.Cofree (CofreeF (..))
import Control.Monad.State.Strict
  ( State,
    gets,
    modify,
    runState,
    withState,
  )
import Data.Bifunctor qualified
import Data.Functor.Foldable (ListF (..), Recursive (..))
import Data.HashMap.Strict ((!?))
import Data.HashMap.Strict qualified as H
import Data.Hashable (Hashable)
import Data.Maybe (fromJust)
import Error (CompileResult)
import ErrorTypes (ErrorMessage, Span)
import GHC.Generics (Generic)
import Predicate (IndexedPredicate)
import ReadTypes (SrcLocked)
import Utils
  ( Between (..),
    Cata,
    Para,
    isDollarSignVar,
    popBetweenTail,
    popTrailingC1Index,
  )
import Var (Var)

requestConstructor0 :: CompileRequest (SrcLocked Ast0.Ast)
requestConstructor0 ds =
  case ds.constructor0 of
    Just constructor0 -> Right (constructor0, ds)
    Nothing -> do
      errorsToEither $ analyzeDefinitionSyntax ds.definition
      let (constructor0, pattern0) = case ds.definition of
            _ C.:< Ast0.CompoundF [_defSymbol, pattern0, constructor0] ->
              (constructor0, pattern0)
            _ -> unreachableBecauseOfAnalysisStep "analyzeDefinitionSyntax"
          ds' = ds {constructor0 = Just constructor0, pattern0 = Just pattern0}
      Right (constructor0, ds')

requestPattern0 :: CompileRequest (SrcLocked Ast0.Ast)
requestPattern0 ds =
  case ds.pattern0 of
    Just pattern0 -> Right (pattern0, ds)
    Nothing -> do
      (_constructor0, ds) <- requestConstructor0 ds
      Right (fromJust ds.pattern0, ds)

requestConstructor1 :: CompileRequest (SrcLocked Ast1.Ast)
requestConstructor1 ds =
  case ds.constructor1 of
    Just constructor1 -> Right (constructor1, ds)
    Nothing -> do
      (constructor0, ds) <- requestConstructor0 ds
      (pattern0, ds) <- requestPattern0 ds
      let constructor1 = compile0to1 constructor0
          pattern1 = compile0to1 pattern0
          errors =
            analyzePatternForMoreThan1EllipsisPerTerm pattern1
              <> analyzeEllipsesAppliedToSymbols pattern1
              <> analyzeEllipsesAppliedToSymbols constructor1
              <> analyzeVariableNotMatchedInPattern pattern1 constructor1
              <> analyzeEllipsesCapturesWithoutVariables pattern1
              <> analyzeEllipsesCapturesWithoutVariables constructor1
              <> analyzeVariablesUsedMoreThanOnceInPattern pattern1
      errorsToEither errors
      let ds' =
            ds
              { constructor1 = Just constructor1,
                pattern1 = Just pattern1
              }
      Right (constructor1, ds')

requestPattern1 :: CompileRequest (SrcLocked Ast1.Ast)
requestPattern1 ds =
  case ds.pattern1 of
    Just pattern1 -> Right (pattern1, ds)
    Nothing -> do
      (_constructor1, ds) <- requestConstructor1 ds
      Right (fromJust ds.pattern1, ds)

requestPatternP0 :: CompileRequest (SrcLocked AstP0.Ast)
requestPatternP0 ds =
  case ds.patternP0 of
    Just patternP0 -> Right (patternP0, ds)
    Nothing -> do
      (pattern1, ds) <- requestPattern1 ds
      let patternP0 = compile1toP0 pattern1
          ds' = ds {patternP0 = Just patternP0}
      Right (patternP0, ds')

requestVariableBindings :: CompileRequest VariableBindings
requestVariableBindings ds =
  case ds.variableBindings of
    Just variableBindings -> Right (variableBindings, ds)
    Nothing -> do
      (patternP0, ds) <- requestPatternP0 ds
      let variableBindings = p0VariableBindings patternP0
          ds' = ds {variableBindings = Just variableBindings}
      Right (variableBindings, ds')

requestConstructorC0 :: CompileRequest (SrcLocked AstC0.Ast)
requestConstructorC0 ds =
  case ds.constructorC0 of
    Just constructorC0 -> Right (constructorC0, ds)
    Nothing -> do
      ((constructor1, variableBindings), ds2) <- request2 requestConstructor1 requestVariableBindings ds
      let constructorC0 = compile1toC0 variableBindings constructor1
      (pattern1, ds3) <- requestPattern1 ds2
      errorsToEither $ analyzeEllipsesCaptures pattern1 constructorC0
      let ds' = ds3 {constructorC0 = Just constructorC0}
      Right (constructorC0, ds')

requestConstructorC1 :: CompileRequest (SrcLocked AstC1.Ast)
requestConstructorC1 ds =
  case ds.constructorC1 of
    Just constructorC1 -> Right (constructorC1, ds)
    Nothing -> do
      (constructorC0, ds) <- requestConstructorC0 ds
      let (constructorC1, nextUnusedVar) = compileC0ToC1 constructorC0
          ds' = ds {constructorC1 = Just constructorC1, nextUnusedVar}
      Right (constructorC1, ds')

requestConstructorC2 :: CompileRequest (SrcLocked (AstC2.Ast Int))
requestConstructorC2 ds =
  case ds.constructorC2 of
    Just constructorC2 -> Right (constructorC2, ds)
    Nothing -> do
      (constructorC1, ds) <- requestConstructorC1 ds
      let (constructorC2WithNamedLabels, nextUnusedVar) =
            compileC1ToC2 ds.nextUnusedVar constructorC1
          constructorC2WithOffsetLabels =
            resolveC2NamedLabels constructorC2WithNamedLabels
          ds' =
            ds
              { constructorC2 = Just constructorC2WithOffsetLabels,
                nextUnusedVar
              }
      Right (constructorC2WithOffsetLabels, ds')

requestPredicates :: CompileRequest [IndexedPredicate]
requestPredicates ds =
  case ds.predicates of
    Just predicates -> Right (predicates, ds)
    Nothing -> do
      (variableBindings, ds) <- requestVariableBindings ds
      (patternP0, ds) <- requestPatternP0 ds
      let predicates = ruleDefinitionPredicates variableBindings patternP0
          ds' = ds {predicates = Just predicates} :: DefinitionStorage
      Right (predicates, ds')

-- Perform two requests at once, trying both even if one fails. This makes it
-- a bit easier to report multiple errors to the user without just stopping
-- after the first one.
request2 :: CompileRequest a -> CompileRequest b -> CompileRequest (a, b)
request2 req1 req2 ds =
  case req1 ds of
    Left errors1 -> case req2 ds of
      Left errors2 -> Left (errors1 <> errors2)
      Right _a2 -> Left errors1
    Right (a1, ds) -> case req2 ds of
      Left errors2 -> Left errors2
      Right (a2, ds) -> Right ((a1, a2), ds)

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

compile1toP0 :: SrcLocked Ast1.Ast -> SrcLocked AstP0.Ast
compile1toP0 = para go
  where
    go :: Para (SrcLocked Ast1.Ast) (SrcLocked AstP0.Ast)
    go (span :< ast) = case ast of
      Ast1.SymbolF s -> span C.:< AstP0.SymbolF s
      Ast1.CompoundF inputXsPairs ->
        let wasEllipses :: (SrcLocked Ast1.Ast, SrcLocked AstP0.Ast) -> Bool
            wasEllipses = \case
              (_ C.:< Ast1.EllipsesF _, _) -> True
              _ -> False
         in case splitBeforeAndAfter wasEllipses inputXsPairs of
              Nothing ->
                span C.:< AstP0.CompoundWithoutEllipsesF (map snd inputXsPairs)
              Just (b, e, a) ->
                span C.:< AstP0.CompoundWithEllipsesF (map snd b) (snd e) (map snd a)
      Ast1.EllipsesF x -> extract x

p0VariableBindings :: SrcLocked AstP0.Ast -> VariableBindings
p0VariableBindings = cata go . indexP0ByC0
  where
    go :: Cata (Cofree AstP0.AstF (Span Int, AstC0.Index)) VariableBindings
    go ((l, index) :< ast) = case ast of
      AstP0.SymbolF s ->
        if isDollarSignVar s
          then H.singleton s (index, l)
          else H.empty
      AstP0.CompoundWithoutEllipsesF xs -> H.unions xs
      AstP0.CompoundWithEllipsesF b e a -> H.unions $ e : (b <> a)

errorsToEither :: [ErrorMessage] -> CompileResult ()
errorsToEither = \case
  [] -> Right ()
  errors -> Left errors

compile0to1 :: SrcLocked Ast0.Ast -> SrcLocked Ast1.Ast
compile0to1 = cata go
  where
    go (span :< ast) = case ast of
      Ast0.SymbolF s -> span C.:< Ast1.SymbolF s
      Ast0.CompoundF xs -> span C.:< Ast1.CompoundF (nestEllipses xs)
        where
          nestEllipses :: [SrcLocked Ast1.Ast] -> [SrcLocked Ast1.Ast]
          nestEllipses ((span1 C.:< y) : (span2 C.:< Ast1.SymbolF "..") : ys) = nestEllipses (y' : ys)
            where
              y' :: SrcLocked Ast1.Ast
              y' = span2 C.:< Ast1.EllipsesF (span1 C.:< y)
          nestEllipses (y : ys) = y : nestEllipses ys
          nestEllipses [] = []

compile1toC0 :: VariableBindings -> SrcLocked Ast1.Ast -> SrcLocked AstC0.Ast
compile1toC0 variableBindings = cata go
  where
    go :: Cata (SrcLocked Ast1.Ast) (SrcLocked AstC0.Ast)
    go (span :< ast) = case ast of
      Ast1.SymbolF s -> case H.lookup s variableBindings of
        Nothing -> span C.:< AstC0.SymbolF s
        Just (index, _varSpan) -> span C.:< AstC0.VariableF index s
      Ast1.CompoundF xs -> span C.:< AstC0.CompoundF xs
      Ast1.EllipsesF x -> span C.:< AstC0.EllipsesF x

data C0ToC1Data = C0ToC1Data
  { ast :: !(SrcLocked AstC1.Ast),
    nextUnusedVar :: !Var,
    remainingAssignment :: Maybe (Var, AstC0.Index, Between)
  }

compileC0ToC1 :: SrcLocked AstC0.Ast -> (SrcLocked AstC1.Ast, Var)
compileC0ToC1 ast =
  let firstUnusedVar :: Var
      firstUnusedVar = 0
      d = cata traverseC0ToC1 ast firstUnusedVar
   in case d.remainingAssignment of
        Just _ -> error "unreachable due to analyzeEllipsesCounts"
        Nothing -> (d.ast, d.nextUnusedVar)

traverseC0ToC1 :: Cata (SrcLocked AstC0.Ast) (Var -> C0ToC1Data)
traverseC0ToC1 (l :< a) nextUnusedVar = case a of
  AstC0.SymbolF s ->
    C0ToC1Data
      { ast = l C.:< AstC1.SymbolF s,
        nextUnusedVar = nextUnusedVar,
        remainingAssignment = Nothing
      }
  AstC0.VariableF i _s ->
    let (c0, c1) = popTrailingC1Index i
        copyAst = l C.:< AstC1.CopyF nextUnusedVar
     in C0ToC1Data
          { ast =
              if null c1
                then copyAst
                else
                  let location = if null c0 then TopLevel else NotTopLevel
                   in l C.:< AstC1.AssignmentF (nextUnusedVar, c1, location) copyAst,
            nextUnusedVar = nextUnusedVar + 1,
            remainingAssignment =
              if null c0
                then Nothing
                else Just $
                  case popBetweenTail c0 of
                    (c0', Just between) ->
                      (nextUnusedVar, c0', between)
                    _ -> error "unreachable"
          }
  AstC0.EllipsesF x ->
    let C0ToC1Data ast nextUnusedVar' remainingAssignment = x nextUnusedVar
     in case remainingAssignment of
          Nothing -> unreachableBecauseOfAnalysisStep "analyzeEllipsesCounts"
          Just (var, c0, Between {zeroPlus, lenMinus}) ->
            let (c0', c1) = popTrailingC1Index c0
                loopAst =
                  l
                    C.:< AstC1.LoopF
                      { AstC1.varF = var,
                        AstC1.srcF = nextUnusedVar' + 1,
                        AstC1.startF = zeroPlus,
                        AstC1.endF = lenMinus,
                        AstC1.bodyF = ast
                      }
             in C0ToC1Data
                  { ast =
                      if null c1
                        then
                          if null c0'
                            then l C.:< AstC1.AssignmentF (nextUnusedVar' + 1, c1, TopLevel) loopAst
                            else loopAst
                        else
                          let location = if null c0' then TopLevel else NotTopLevel
                           in l C.:< AstC1.AssignmentF (nextUnusedVar' + 1, c1, location) loopAst,
                    nextUnusedVar = nextUnusedVar' + 2,
                    remainingAssignment =
                      if null c0'
                        then Nothing
                        else Just $
                          case popBetweenTail c0' of
                            (c0', Just between) ->
                              (nextUnusedVar + 1, c0', between)
                            _ -> error "unreachable"
                  }
  AstC0.CompoundF xs -> cata mergeXS xs nextUnusedVar
    where
      mergeXS :: Cata [Var -> C0ToC1Data] (Var -> C0ToC1Data)
      mergeXS Nil nextUnusedVar =
        C0ToC1Data
          { ast = l C.:< AstC1.CompoundF [],
            nextUnusedVar,
            remainingAssignment = Nothing
          }
      mergeXS (Cons x xs) nextUnusedVar =
        let xData = x nextUnusedVar
            xsData = xs xData.nextUnusedVar
            remainingAssignment'' =
              compatibleRemainingAssignment
                xData.remainingAssignment
                xsData.remainingAssignment
            compoundInternals =
              case xsData.ast of
                _l C.:< AstC1.CompoundF compoundInternals -> compoundInternals
                _ -> error "unreachable"
            ast' = l C.:< AstC1.CompoundF (xData.ast : compoundInternals)
         in C0ToC1Data
              { ast = ast',
                nextUnusedVar = xsData.nextUnusedVar,
                remainingAssignment = remainingAssignment''
              }
        where
          compatibleRemainingAssignment ::
            Maybe (Var, AstC0.Index, Between) ->
            Maybe (Var, AstC0.Index, Between) ->
            Maybe (Var, AstC0.Index, Between)
          compatibleRemainingAssignment Nothing Nothing = Nothing
          compatibleRemainingAssignment (Just t) Nothing = Just t
          compatibleRemainingAssignment Nothing (Just t) = Just t
          compatibleRemainingAssignment (Just t) (Just u) =
            if t == u
              then Just u
              else unreachableBecauseOfAnalysisStep "analyzeEllipsesCaptures"

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

cofreeConcat :: b -> [Cofree (ListF a) b] -> Cofree (ListF a) b
cofreeConcat l = foldr cofreeAppend (l C.:< Nil)

compileC1ToC2 :: Var -> SrcLocked AstC1.Ast -> (SrcLocked (AstC2.Ast NamedLabel), Var)
compileC1ToC2 nextUnusedVar ast = runStateAndReturnResults (para go ast) initialState
  where
    runStateAndReturnResults ::
      State C1ToC2InputData (SrcLocked (AstC2.Ast NamedLabel)) ->
      C1ToC2InputData ->
      (SrcLocked (AstC2.Ast NamedLabel), Var)
    runStateAndReturnResults state initialState =
      let (astC2, c1ToC2InputData) = runState state initialState
       in (astC2, c1ToC2InputData._c2iNextUnusedVar)

    initialState :: C1ToC2InputData
    initialState =
      C1ToC2InputData
        { _c2iNextUnusedVar = nextUnusedVar,
          _c2iCompoundTermLengthCounter = Nothing
        }
    isC1NonLoopVariant :: SrcLocked AstC1.Ast -> Bool
    isC1NonLoopVariant (_ C.:< AstC1.LoopF {}) = False
    isC1NonLoopVariant (_ C.:< AstC1.AssignmentF _ x) = isC1NonLoopVariant x
    isC1NonLoopVariant _ = True

    -- We use 'para' instead of 'cata' because in the 'CompoundF' case, we
    -- need to be able to count the number of non-loops in the subterms.
    go :: Para (SrcLocked AstC1.Ast) (State C1ToC2InputData (SrcLocked (AstC2.Ast NamedLabel)))
    go (span :< ast) = case ast of
      AstC1.SymbolF s -> pure $ mapSrcLock span [AstC2.Push $ C2Expr.Symbol s]
      AstC1.CompoundF inputXsPairs -> do
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
            q2 = cofreeConcat span <$> q1
        xs <- q2
        let inputs :: [SrcLocked AstC1.Ast]
            inputs = map fst inputXsPairs'
            numNonLoopInputs = length . filter isC1NonLoopVariant $ inputs
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
          ( mapSrcLock span [initLengthCountVar]
              `cofreeAppend` xs
              `cofreeAppend` mapSrcLock span [buildCompoundTerm]
          )
      AstC1.AssignmentF (var, index, loc) inputXPair -> do
        x <- snd inputXPair
        let assignmentStmts = indexAssignStmts span var loc index
        pure $ assignmentStmts `cofreeAppend` x
      AstC1.CopyF v -> pure $ mapSrcLock span [AstC2.Push $ C2Expr.Var v]
      AstC1.LoopF var src start end inputXPair -> do
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
                span
                [ assignLoopCountVar,
                  assignLoopEndVar,
                  jumpBot,
                  assignVarToSrc
                ]
            srcLockedEpilogue =
              mapSrcLock
                span
                [ incremeentLoopCountVar,
                  incremenetLengthCountVar,
                  jumpTop
                ]
        pure $ srcLockedPrologue `cofreeAppend` x `cofreeAppend` srcLockedEpilogue

data NamedLabel = TopOfLoop !Int | BotOfLoop !Int deriving (Eq, Generic)

instance Hashable NamedLabel

enumerateCofree :: Cofree (ListF a) b -> Cofree (ListF a) (Int, b)
enumerateCofree cf = cata go cf 0
  where
    go :: Cata (Cofree (ListF a) b) (Int -> Cofree (ListF a) (Int, b))
    go cofree n = case cofree of
      b :< Nil -> (n, b) C.:< Nil
      b :< Cons x xs -> (n, b) C.:< Cons x (xs $ n + 1)

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