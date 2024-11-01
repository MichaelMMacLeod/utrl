module Read (Read.read, cmdLineArgEmitStagesParser) where

import Ast0 qualified
import CompileTypes (mkStorage)
import ConfigTypes (EmitStages (..), onlyStage1, onlyStageC0, onlyStageC1, onlyStageC2, onlyStageP0)
import Control.Comonad.Cofree (Cofree (..))
import Data.Char (isSpace)
import Data.Kind (Type)
import Data.Text (Text, pack)
import Data.Void (Void)
import Error
  ( CompileResult (..),
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
    Right ts ->
      CompileResult
        { storage = mkStorage ts,
          result = Right ts
        }
    Left parseErrorBundle ->
      CompileResult
        { storage = mkStorage [], -- TODO, maintain correctly parsed asts (if any)
          result = Left [parseErrorMessage parseErrorBundle]
        }

type Parser :: Type -> Type
type Parser = Parsec Void Text

cmdLineArgEmitStagesParser :: Parser EmitStages
cmdLineArgEmitStagesParser = do
  spaceConsumer
  em <- many emitStagesParser
  eof
  pure $ mconcat em

emitStagesParser :: Parser EmitStages
emitStagesParser =
  stage1Parser
    <|> stageC0Parser
    <|> stageC1Parser
    <|> stageC2Parser
    <|> stageP0Parser
    <|> stageP0Parser

stage1Parser :: Parser EmitStages
stage1Parser = lexemeComma "1" >> pure onlyStage1

stageC0Parser :: Parser EmitStages
stageC0Parser = lexemeComma "C0" >> pure onlyStageC0

stageC1Parser :: Parser EmitStages
stageC1Parser = lexemeComma "C1" >> pure onlyStageC1

stageC2Parser :: Parser EmitStages
stageC2Parser = lexemeComma "C2" >> pure onlyStageC2

stageP0Parser :: Parser EmitStages
stageP0Parser = lexemeComma "P0" >> pure onlyStageP0

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

lexemeComma :: Parser a -> Parser a
lexemeComma p = do
  l <- lexeme p
  _ <- many $ lexeme $ symbol ","
  pure l

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")