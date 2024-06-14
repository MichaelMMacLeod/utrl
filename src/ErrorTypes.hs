module ErrorTypes
  ( ErrorType (..),
    ErrorMessageInfo (..),
    Annotation (..),
    Span (..),
    ErrorMessage,
  )
where

import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)

type ErrorType :: Type
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
  deriving stock (Eq, Show)

type ErrorMessage :: Type
type ErrorMessage = ErrorMessageInfo Int

type ErrorMessageInfo :: Type -> Type
data ErrorMessageInfo l = ErrorMessageInfo
  { errorType :: ErrorType,
    message :: Text,
    annotations :: [Annotation l],
    help :: Maybe Text
  }
  deriving stock (Eq, Show)

type Annotation :: Type -> Type
data Annotation l = Annotation
  { span :: Span l,
    annotation :: Text
  }
  deriving stock (Eq, Show)

type Span :: Type -> Type
data Span l = Span
  { location :: l,
    length :: Int
  }
  deriving stock (Eq, Show, Generic)

instance Hashable (Span Int)