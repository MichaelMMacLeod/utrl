{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-
The functions in this file perform semantic analysis on various stages of compilation,
returning informative error messages should errors be found.
-}

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
import Data.Maybe (catMaybes, fromJust, listToMaybe)
import Error (badEllipsesCapturesErrorMessage, badEllipsesCountErrorMessage)
import ErrorTypes (ErrorMessage, ErrorMessageInfo, Span)
import ReadTypes (SrcLocked)
import Utils
  ( Between,
    Cata,
    getPatternSpanAtC0Index,
    popBetweenTail,
    popTrailingC1Index,
    pushBetweenTail,
  )
import Prelude hiding (span)

-- | Finds errors relating to the use of too few or too many ellipses
analyzeEllipsesCounts :: VariableBindings -> SrcLocked AstC0.Ast -> [ErrorMessage]
analyzeEllipsesCounts variableBindings ast = cata go ast 0
  where
    go :: Cata (SrcLocked AstC0.Ast) (Int -> [ErrorMessageInfo Int])
    go cofree actualEllipsesCount = case cofree of
      _ :< AstC0.SymbolF _ -> []
      _ :< AstC0.CompoundF xs -> concatMap ($ actualEllipsesCount) xs
      constructorVarSpan :< AstC0.VariableF x s ->
        let requiredCount = requiredEllipses x
            (_, patternVarSpan) = fromJust $ variableBindings H.!? s
            errorMessage =
              badEllipsesCountErrorMessage
                requiredCount
                actualEllipsesCount
                patternVarSpan
                constructorVarSpan
         in [errorMessage | requiredCount /= actualEllipsesCount]
      _ :< AstC0.EllipsesF x -> x $ actualEllipsesCount + 1
    requiredEllipses :: AstC0.Index -> Int
    requiredEllipses = length . filter AstC0.isBetween

-- | Finds errors relating to the use of variables under the same ellipsis that
-- weren't matched under the same ellipsis.
--
-- For example, '(def ((x ..) (y ..)) ((x y) ..))' is detected as erroneous here. The
-- problem with this example is that the number of terms matched to 'x' may be different
-- from the number of terms matched to 'y', so it is not in general possible to create
-- '(x y) ..'.
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

data Assignment = Assignment
  { variableName :: String,
    variableSpan :: Span Int,
    index :: (AstC0.Index, Between)
  }
  deriving (Show)