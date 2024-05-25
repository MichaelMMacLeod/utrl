module Read (Read.read, read', SrcLocked, SrcLockedF) where

import Ast0 qualified
import Control.Comonad.Cofree (Cofree (..))
import Control.Comonad.Trans.Cofree (CofreeF)
import Data.Char (isSpace)
import Data.Either.Extra (fromRight')
import Data.Functor.Foldable (Base)
import Data.Text (Text, unpack)
import Data.Void (Void)
import Error
  ( CompileResult,
    FileContents,
    mkFilePathName,
    parseErrorMessage,
  )
import Text.Megaparsec
  ( Parsec,
    between,
    eof,
    getOffset,
    many,
    runParser,
    satisfy,
    (<|>),
  )
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (span)
import Utils (Span (..))

type SrcLocked t = Cofree (Base t) (Span Int)

type SrcLockedF t = CofreeF (Base t) (Span Int)

read :: Maybe FilePath -> FileContents -> CompileResult [SrcLocked Ast0.Ast]
read name contents =
  case runParser terms (mkFilePathName name) contents of
    Right ts -> Right ts
    Left parseErrorBundle -> Left $ parseErrorMessage parseErrorBundle

-- Partial read, which errors at runtime on compile errors. Useful for reducing
-- boilerplate for tests.
read' :: Text -> [SrcLocked Ast0.Ast]
read' = fromRight' . Read.read Nothing

type Parser = Parsec Void Text

terms :: Parser [SrcLocked Ast0.Ast]
terms = do
  spaceConsumer
  ts <- many term
  eof
  pure ts

term :: Parser (SrcLocked Ast0.Ast)
term = rwSymbol <|> compoundTerm

compoundTerm :: Parser (SrcLocked Ast0.Ast)
compoundTerm = do
  offset <- getOffset
  ts <- between leftParen rightParen (many term)
  endingOffset <- getOffset
  let span = Span offset (endingOffset - offset)
  pure $ span :< Ast0.CompoundF ts

leftParen :: Parser ()
leftParen = do
  _ <- lexeme $ symbol "("
  pure ()

rightParen :: Parser ()
rightParen = do
  _ <- lexeme $ symbol ")"
  pure ()

rwSymbolChar :: Parser Char
rwSymbolChar = satisfy $ \c -> not (isSpace c) && c /= '(' && c /= ')'

rwSymbol :: Parser (SrcLocked Ast0.Ast)
rwSymbol = lexeme $ do
  offset <- getOffset
  c0 <- rwSymbolChar
  cN <- many rwSymbolChar
  endingOffset <- getOffset
  let span = Span offset (endingOffset - offset)
  pure $ span :< Ast0.SymbolF (c0 : cN)

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")