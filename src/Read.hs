{-# LANGUAGE TypeFamilies #-}

module Read (Read.read) where

import qualified Ast0
import Error (CompileError)
import qualified Error
import Lex (Token (..), lex)

read :: String -> Either CompileError Ast0.Ast
read = parse . Lex.lex

parse :: [Token] -> Either CompileError Ast0.Ast
parse xs = go xs []
  where
    go :: [Token] -> [[Ast0.Ast]] -> Either CompileError Ast0.Ast
    go (TLeft : ts) acc = go ts ([] : acc)
    go (TRight : ts) (a1 : a2 : acc) = let c = Ast0.Compound (reverse a1) in go ts ((c : a2) : acc)
    go (TRight : _) [a1] = Right $ Ast0.Compound (reverse a1)
    go (TRight : _) [] = Left Error.ExpectedLeftParen
    go (TSymbol s : ts) (a : acc) = go ts ((Ast0.Symbol s : a) : acc)
    go (TSymbol s : _) [] = Right $ Ast0.Symbol s
    go [] _ = Left Error.NoInput