module Read (Read.read, read') where

import Ast0 qualified
import Control.Comonad.Cofree (Cofree)
import Data.Char (isSpace)
import Data.Either.Extra (fromRight')
import Data.Functor.Foldable (Base)
import Data.Text (Text, unpack)
import Data.Void (Void)
import Error (CompileResult, FileContents, Filename, parseErrorMessage)
import Text.Megaparsec (Parsec, between, eof, many, runParser, satisfy, (<|>))
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

type SrcLocked t = Cofree (Base t) Int

-- readWithAnnotations :: Filename -> Text -> CompileResult [Ann Ast0.Ast]
-- readWithAnnotations source input = _

data Token = LeftParen | RightParen | Symbol !Text

read :: Filename -> FileContents -> CompileResult [Ast0.Ast]
read name contents =
  case runParser terms (unpack name) contents of
    Right ts -> Right ts
    Left parseErrorBundle -> Left $ parseErrorMessage parseErrorBundle

-- Partial read, which errors at runtime on compile errors. Useful for reducing
-- boilerplate for tests.
read' :: Text -> [Ast0.Ast]
read' = fromRight' . Read.read "unknown_file_name"

type Parser = Parsec Void Text

-- rwAst :: Parser Ast0.Ast
-- rwAst = do
--   ts <- tokens
--   _

-- tokens :: Parser [Token]
-- tokens = do
--   spaceConsumer
--   ts <- many token
--   eof
--   pure ts

-- token :: Parser Token
-- token = choice [leftParen, rightParen, rwSymbol]

-- term :: Parser Ast0.Ast
-- term =

terms :: Parser [Ast0.Ast]
terms = do
  spaceConsumer
  ts <- many term
  eof
  pure ts

term :: Parser Ast0.Ast
term = rwSymbol <|> compoundTerm

compoundTerm :: Parser Ast0.Ast
compoundTerm = do
  ts <- between leftParen rightParen (many term)
  pure $ Ast0.Compound ts

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

rwSymbol :: Parser Ast0.Ast
rwSymbol = lexeme $ do
  c0 <- rwSymbolChar
  cN <- many rwSymbolChar
  pure . Ast0.Symbol $ c0 : cN

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

-- read :: Text -> CompileResult [Ast0.Ast]
-- -- read input = mapLeft (genericErrorInfo ParsingError) (parseRW input)
-- read input = case parseRW input of
--   Left e -> Left (genericErrorInfo ParsingError)
--   Right s -> Right s

-- parseRW :: Text -> Either ParseError [Ast0.Ast]
-- parseRW = parse rwFile ""

-- rwFile :: Parsec Text () [Ast0.Ast]
-- rwFile = do
--   skipMany space
--   ast <- many term
--   eof
--   pure ast

-- term :: Parsec Text () Ast0.Ast
-- term = do
--   t <- choice [compoundTerm, symbolTerm]
--   skipMany space
--   pure t

-- compoundTerm :: Parsec Text () Ast0.Ast
-- compoundTerm =
--   Ast0.Compound
--     <$> between
--       (char '(')
--       (char ')')
--       ( do
--           skipMany space
--           many term
--       )

-- symbolTerm :: Parsec Text () Ast0.Ast
-- symbolTerm = Ast0.Symbol <$> many1 symbolChar

-- symbolChar :: Parsec Text () Char
-- symbolChar = satisfy $ \c -> not (isSpace c) && c /= '(' && c /= ')'