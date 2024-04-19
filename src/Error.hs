module Error (CompileError(..), CompileResult) where
import Text.Parsec (ParseError)

type CompileResult a = Either CompileError a

data CompileError
  = ParsecParseError ParseError
  | NoInput
  | ExpectedLeftParen
  | TooFewEllipsesInConstructor
  | TooManyEllipsesInConstructor
  | VarsNotCapturedUnderSameEllipsisInConstructor
  | EllipsisAppliedToSymbolInConstructor
  | InvalidRuleDefinition
  | MoreThanOneEllipsisInSingleCompoundTermOfPattern
  | VariableUsedMoreThanOnceInPattern
  deriving (Eq, Show)