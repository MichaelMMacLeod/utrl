module ErrorTypes
  ( ErrorType (..),
    ErrorMessageInfo (..),
    Annotation (..),
    Span (..),
    ErrorMessage,
  )
where
import Data.Text (Text)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data ErrorType
  = ParsingError
  | BadEllipsesCount
  | VarsNotCapturedUnderSameEllipsisInConstructor
  | EllipsisAppliedToSymbol
  | MoreThanOneEllipsisInSingleCompoundTermOfPattern
  | VariableUsedMoreThanOnceInPattern
  | OverlappingPatterns
  | NoVariablesInEllipsis
  | ExpectedDefinitionGotSymbol
  | DefinitionHasWrongNumberOfTerms
  | DefinitionDoesNotStartWithDef
  | VariableNotMatchedInPattern
  deriving (Eq, Show)

type ErrorMessage = ErrorMessageInfo Int

data ErrorMessageInfo l = ErrorMessageInfo
  { errorType :: ErrorType,
    message :: Text,
    annotations :: [Annotation l],
    help :: Maybe Text
  }
  deriving (Eq, Show)

data Annotation l = Annotation
  { span :: Span l,
    annotation :: Text
  }
  deriving (Eq, Show)

data Span l = Span
  { location :: l,
    length :: Int
  }
  deriving (Eq, Show, Generic)

instance Hashable (Span Int)