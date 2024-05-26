module Analyze
  ( analyzeC0EllipsesCounts,
  )
where

import AstC0 qualified
import CompileTypes (VariableBindings)
import Control.Comonad.Trans.Cofree (CofreeF (..))
import Data.Functor.Foldable (Recursive (..))
import Data.HashMap.Strict qualified as H
import Data.Maybe (fromJust)
import Error (badEllipsesCountErrorMessage)
import ErrorTypes (ErrorMessageInfo)
import Read (SrcLocked)
import Utils (Cata)

analyzeC0EllipsesCounts :: VariableBindings -> SrcLocked AstC0.Ast -> [ErrorMessageInfo Int]
analyzeC0EllipsesCounts variableBindings ast = cata go ast 0
  where
    go :: Cata (SrcLocked AstC0.Ast) (Int -> [ErrorMessageInfo Int])
    go cofree actualEllipsesCount = case cofree of
      _ :< AstC0.SymbolF _ -> []
      _ :< AstC0.CompoundF xs -> concatMap ($ actualEllipsesCount) xs
      constructorVarSpan :< AstC0.VariableF (x, s) ->
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