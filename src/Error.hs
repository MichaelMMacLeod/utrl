module Error (CompileError(..), CompileResult) where

type CompileResult a = Either CompileError a

data CompileError
  = NoInput
  | ExpectedLeftParen
  | TooFewEllipsesInConstructor
  | TooManyEllipsesInConstructor
  | VarsNotCapturedUnderSameEllipsisInConstructor
  | EllipsisAppliedToSymbolInConstructor
  | InvalidRuleDefinition
  | MoreThanOneEllipsisInSingleCompoundTermOfPattern
  deriving (Eq, Show)