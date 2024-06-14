module Read (Read.read, read') where

import Ast0 qualified
import Control.Comonad.Cofree (Cofree (..))
import Data.Char (isSpace)
import Data.Either.Extra (fromRight')
import Data.Text (Text, pack)
import Data.Void (Void)
import Error
  ( CompileResult,
    FileContents,
    mkFilePathName,
    parseErrorMessage,
  )
import ErrorTypes (Span (..))
import ReadTypes (SrcLocked)
import Text.Megaparsec
  ( Parsec,
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

read :: Maybe FilePath -> FileContents -> CompileResult [SrcLocked Ast0.Ast]
read name contents =
  case runParser terms (mkFilePathName name) contents of
    Right ts -> Right ts
    Left parseErrorBundle -> Left $ [parseErrorMessage parseErrorBundle]

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
term = utrlSymbol <|> compoundTerm

compoundTerm :: Parser (SrcLocked Ast0.Ast)
compoundTerm = do
  offset <- getOffset
  ts <- leftParen *> many term
  endingOffset <- fmap (+ 1 {- to include the rightParen -}) getOffset
  rightParen
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

utrlSymbolChar :: Parser Char
utrlSymbolChar = satisfy $ \c -> not (isSpace c) && c /= '(' && c /= ')'

utrlSymbol :: Parser (SrcLocked Ast0.Ast)
utrlSymbol = lexeme $ do
  offset <- getOffset
  c0 <- utrlSymbolChar
  cN <- many utrlSymbolChar
  endingOffset <- getOffset
  let span = Span offset (endingOffset - offset)
  pure $ span :< Ast0.SymbolF (pack $ c0 : cN)

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