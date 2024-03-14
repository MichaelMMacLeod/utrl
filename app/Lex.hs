{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Lex (Lex.lex, Token (..)) where

import Control.Applicative (liftA2)
import Data.Char (isSpace)
import Data.Functor.Foldable (ListF (..), ana)

data Token = TLeft | TRight | TSymbol String deriving (Show)

lex :: String -> [Token]
lex = ana $ \case
  [] -> Nil
  xs -> let (token, rest) = parseToken xs in Cons token rest
    where
      parseToken :: String -> (Token, String)
      parseToken (' ' : xs) = parseToken xs
      parseToken ('(' : xs) = (TLeft, xs)
      parseToken (')' : xs) = (TRight, xs)
      parseToken xs = (TSymbol $ takeWhile isSymbolChar xs, dropWhile isSymbolChar xs)

      isSymbolChar :: Char -> Bool
      isSymbolChar =
        notF isSpace
          <&&> notF (== '(')
          <&&> notF (== ')')
        where
          notF = fmap not
          (<&&>) = liftA2 (&&)