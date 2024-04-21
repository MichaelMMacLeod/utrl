module Read (parseRW, Read.read, read') where

import qualified Ast0
import Control.Comonad.Identity (Identity)
import Data.Char (isSpace)
import Data.Either.Extra (fromRight', mapLeft)
import Data.Text (Text)
import Error (CompileError (..), CompileResult)
import Text.Parsec (ParseError, Parsec, ParsecT, between, choice, eof, many, many1, satisfy, sepBy, skipMany, skipMany1, space, spaces, (<?>))
import Text.Parsec.Char (char)
import Text.ParserCombinators.Parsec (parse)

read :: Text -> CompileResult [Ast0.Ast]
read input = mapLeft ParsecParseError (parseRW input)

-- Partial read, which errors at runtime on compile errors. Useful for reducing
-- boilerplate for tests.
read' :: Text -> [Ast0.Ast]
read' = fromRight' . Read.read

parseRW :: Text -> Either ParseError [Ast0.Ast]
parseRW = parse rwFile ""

rwFile :: Parsec Text () [Ast0.Ast]
rwFile = do
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