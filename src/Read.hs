module Read (parseRW, Read.read, read') where

import Ast0 qualified
import Data.Char (isSpace)
import Data.Either.Extra (fromRight', mapLeft)
import Data.Text (Text)
import Error (CompileResult, ErrorType (..), genericErrorInfo, Span, Filename)
import Text.Parsec (ParseError, Parsec, between, choice, eof, many, many1, satisfy, skipMany, space)
import Text.Parsec.Char (char)
import Text.ParserCombinators.Parsec (parse)
import Control.Comonad.Cofree (Cofree)
import Data.Functor.Foldable (Base)

type Ann t = Cofree (Base t) Span

-- readWithAnnotations :: Filename -> Text -> CompileResult [Ann Ast0.Ast]
-- readWithAnnotations source input = _

read :: Text -> CompileResult [Ast0.Ast]
-- read input = mapLeft (genericErrorInfo ParsingError) (parseRW input)
read input = case parseRW input of
  Left e -> Left (genericErrorInfo ParsingError)
  Right s -> Right s

-- Partial read, which errors at runtime on compile errors. Useful for reducing
-- boilerplate for tests.
read' :: Text -> [Ast0.Ast]
read' = fromRight' . Read.read

parseRW :: Text -> Either ParseError [Ast0.Ast]
parseRW = parse rwFile ""

rwFile :: Parsec Text () [Ast0.Ast]
rwFile = do
  skipMany space
  ast <- many term
  eof
  pure ast

term :: Parsec Text () Ast0.Ast
term = do
  t <- choice [compoundTerm, symbolTerm]
  skipMany space
  pure t

compoundTerm :: Parsec Text () Ast0.Ast
compoundTerm =
  Ast0.Compound
    <$> between
      (char '(')
      (char ')')
      ( do
          skipMany space
          many term
      )

symbolTerm :: Parsec Text () Ast0.Ast
symbolTerm = Ast0.Symbol <$> many1 symbolChar

symbolChar :: Parsec Text () Char
symbolChar = satisfy $ \c -> not (isSpace c) && c /= '(' && c /= ')'