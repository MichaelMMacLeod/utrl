{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Read (parseRW, Read.read, read') where

import qualified Ast0
import Control.Comonad.Identity (Identity)
import Data.Char (isSpace)
import Data.Either.Extra (fromRight', mapLeft)
import Data.Text (Text, pack)
import Error (CompileError (..), CompileResult)
import Text.Parsec
  ( ParseError,
    Parsec,
    ParsecT,
    between,
    choice,
    eof,
    many,
    many1,
    satisfy,
    sepBy,
    space,
  )
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
  ast <- between (many space) (many space) (term `sepBy` many space)
  eof
  pure ast

term :: ParsecT Text () Identity Ast0.Ast
term = choice [compoundTerm, symbolTerm]

compoundTerm :: ParsecT Text () Identity Ast0.Ast
compoundTerm = Ast0.Compound <$> between (char '(') (char ')') compoundTermInternals

compoundTermInternals :: ParsecT Text () Identity [Ast0.Ast]
compoundTermInternals = between (many space) (many space) (term `sepBy` many1 space)

symbolTerm :: ParsecT Text () Identity Ast0.Ast
symbolTerm = Ast0.Symbol <$> many1 symbolChar

symbolChar :: ParsecT Text () Identity Char
symbolChar = satisfy $ \c -> not (isSpace c) && c /= '(' && c /= ')'