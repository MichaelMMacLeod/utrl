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
      parseToken (' ' : ts) = parseToken ts
      parseToken ('(' : ts) = (TLeft, ts)
      parseToken (')' : ts) = (TRight, ts)
      parseToken ts = (TSymbol $ takeWhile isSymbolChar ts, dropWhile isSymbolChar ts)

      isSymbolChar :: Char -> Bool
      isSymbolChar =
        notF isSpace
          <&&> notF (== '(')
          <&&> notF (== ')')
        where
          notF = (not .)
          (<&&>) = liftA2 (&&)