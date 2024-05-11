module Error (CompileError (..), CompileResult) where

import qualified AstP0
import Text.Parsec (ParseError)

type CompileResult a = Either CompileError a

data CompileError
  = ParsecParseError ParseError
  | TooFewEllipsesInConstructor
  | TooManyEllipsesInConstructor
  | VarsNotCapturedUnderSameEllipsisInConstructor
  | EllipsisAppliedToSymbolInConstructor
  | InvalidRuleDefinition
  | MoreThanOneEllipsisInSingleCompoundTermOfPattern
  | VariableUsedMoreThanOnceInPattern
  | OverlappingPatterns (AstP0.Ast, AstP0.Ast)
  deriving (Eq, Show)