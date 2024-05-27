{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Analyze
  ( analyzeEllipsesCounts,
    analyzeEllipsesCaptures,
  )
where

import Ast1 qualified
import AstC0 qualified
import CompileTypes (VariableBindings)
import Control.Comonad.Trans.Cofree (CofreeF (..))
import Data.Functor.Foldable (Recursive (..))
import Data.HashMap.Strict qualified as H
import Data.List.Extra (snoc)
import Data.Maybe (catMaybes, fromJust, listToMaybe)
import Error (badEllipsesCapturesErrorMessage, badEllipsesCountErrorMessage)
import ErrorTypes (ErrorMessage, ErrorMessageInfo, Span)
import ReadTypes (SrcLocked)
import Utils (Between, Cata, getPatternSpanAtC0Index, isDollarSignVar, popBetweenTail, popTrailingC1Index, pushBetweenTail, uncofree)
import Prelude hiding (span)

analyzeEllipsesCounts :: VariableBindings -> SrcLocked AstC0.Ast -> [ErrorMessage]
analyzeEllipsesCounts variableBindings ast = cata go ast 0
  where
    go :: Cata (SrcLocked AstC0.Ast) (Int -> [ErrorMessageInfo Int])
    go cofree actualEllipsesCount = case cofree of
      _ :< AstC0.SymbolF _ -> []
      _ :< AstC0.CompoundF xs -> concatMap ($ actualEllipsesCount) xs
      constructorVarSpan :< AstC0.VariableF x s ->
        let requiredCount = requiredEllipses x
            (_, paternVarSpan) = fromJust $ variableBindings H.!? s
            errorMessage =
              badEllipsesCountErrorMessage
                requiredCount
                actualEllipsesCount
                paternVarSpan
                constructorVarSpan
         in [errorMessage | requiredCount /= actualEllipsesCount]
      _ :< AstC0.EllipsesF x -> x $ actualEllipsesCount + 1
    requiredEllipses :: AstC0.Index -> Int
    requiredEllipses = length . filter AstC0.isBetween

data VariableUnderEllipses = VariableUnderEllipses
  { variable :: String,
    ellipses :: [Span Int]
  }

findVariablesUnderEllipses :: SrcLocked Ast1.Ast -> [VariableUnderEllipses]
findVariablesUnderEllipses = cata go
  where
    go :: Cata (SrcLocked Ast1.Ast) [VariableUnderEllipses]
    go (span :< ast) = case ast of
      Ast1.SymbolF s -> [variableUnderEllipses | isDollarSignVar s]
        where
          variableUnderEllipses = VariableUnderEllipses {variable = s, ellipses = []}
      Ast1.CompoundF xs -> concat xs
      Ast1.EllipsesF x -> map appendThisEllipsis x
        where
          appendThisEllipsis :: VariableUnderEllipses -> VariableUnderEllipses
          appendThisEllipsis v = v {ellipses = snoc v.ellipses span}

data Assignment = Assignment
  { variableName :: String,
    variableSpan :: Span Int,
    index :: (AstC0.Index, Between)
  }
  deriving (Show)

analyzeEllipsesCaptures :: SrcLocked Ast1.Ast -> SrcLocked AstC0.Ast -> [ErrorMessage]
analyzeEllipsesCaptures pattern = fixup . cata go
  where
    fixup = \case
      Left errors -> errors
      Right _ -> []
    go :: Cata (SrcLocked AstC0.Ast) (Either [ErrorMessage] [Assignment])
    go (span :< ast) = case ast of
      AstC0.SymbolF _ -> Right []
      AstC0.CompoundF xs -> concat <$> sequence xs
      AstC0.EllipsesF eithers -> do
        assignments <- eithers
        case listToMaybe assignments of
          Nothing -> Right []
          Just firstAssignment ->
            let checkAssignment :: Assignment -> Either [ErrorMessage] (Maybe Assignment)
                checkAssignment a =
                  if firstAssignment.index == a.index
                    then
                      let index = fst a.index
                          (c0, _c1) = popTrailingC1Index index
                          (c0', maybeBetween) = popBetweenTail c0
                       in case maybeBetween of
                            Nothing -> Right Nothing
                            Just between ->
                              Right . Just $ a {index = (c0', between)}
                    else
                      let var1Name = firstAssignment.variableName
                          ellipses1PatternSpan =
                            fromJust $
                              getPatternSpanAtC0Index pattern $
                                pushBetweenTail firstAssignment.index
                          var2Name = a.variableName
                          ellipses2PatternSpan =
                            fromJust $
                              getPatternSpanAtC0Index pattern $
                                pushBetweenTail a.index
                          ellipsesConstructorSpan = span
                       in Left
                            [ badEllipsesCapturesErrorMessage
                                var1Name
                                ellipses1PatternSpan
                                var2Name
                                ellipses2PatternSpan
                                ellipsesConstructorSpan
                            ]
             in catMaybes <$> mapM checkAssignment assignments
      AstC0.VariableF index variableName ->
        let (c0, _c1) = popTrailingC1Index index
            (c0', maybeBetween) = popBetweenTail c0
         in Right $ case maybeBetween of
              Nothing -> []
              Just between ->
                [ Assignment
                    { variableName,
                      variableSpan = span,
                      index = (c0', between)
                    }
                ]