{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}

module Read (Read.read) where

import Ast0 qualified
import Data.Functor.Foldable (ListF (..), ana)
import Lex (Token (..), lex)

read :: String -> Ast0.Ast0
read = parse . Lex.lex

parse :: [Token] -> Ast0.Ast0
parse xs = go xs []
  where
    go :: [Token] -> [[Ast0.Ast0]] -> Ast0.Ast0
    go (TLeft : xs) acc = go xs ([] : acc)
    go (TRight : xs) (a1 : a2 : acc) = let c = Ast0.Compound (reverse a1) in go xs ((c : a2) : acc)
    go (TRight : xs) [a1] = Ast0.Compound (reverse a1)
    go (TRight : xs) [] = error "Expected '('"
    go (TSymbol s : xs) (a : acc) = go xs ((Ast0.Symbol s : a) : acc)
    go (TSymbol s : xs) [] = Ast0.Symbol s
