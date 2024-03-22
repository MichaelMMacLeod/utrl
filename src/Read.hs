{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}

module Read (Read.read) where

import Ast0 qualified
import Lex (Token (..), lex)

read :: String -> Ast0.Ast
read = parse . Lex.lex

parse :: [Token] -> Ast0.Ast
parse xs = go xs []
  where
    go :: [Token] -> [[Ast0.Ast]] -> Ast0.Ast
    go (TLeft : ts) acc = go ts ([] : acc)
    go (TRight : ts) (a1 : a2 : acc) = let c = Ast0.Compound (reverse a1) in go ts ((c : a2) : acc)
    go (TRight : _) [a1] = Ast0.Compound (reverse a1)
    go (TRight : _) [] = error "Expected '('"
    go (TSymbol s : ts) (a : acc) = go ts ((Ast0.Symbol s : a) : acc)
    go (TSymbol s : _) [] = Ast0.Symbol s
    go [] _ = error "No input"
