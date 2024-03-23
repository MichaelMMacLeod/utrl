module Lex (Lex.lex, Token (..)) where

import Data.Char (isSpace)

data Token = TLeft | TRight | TSymbol String deriving (Show, Eq)

lex :: String -> [Token]
lex [] = []
lex ('(' : str) = TLeft : Lex.lex str
lex (')' : str) = TRight : Lex.lex str
lex (c : str)
  | isSpace c = Lex.lex str
  | otherwise = TSymbol symPart : Lex.lex otherPart
  where
    symPart, otherPart :: String
    (symPart, otherPart) = span isSymbolChar (c : str)

isSymbolChar :: Char -> Bool
isSymbolChar c = c /= '(' && c /= ')' && not (isSpace c)