module Predicate (Predicate (..), IndexedPredicate (..)) where

import qualified AstC0

data Predicate
  = SymbolEqualTo String
  | LengthEqualTo Int
  | LengthGreaterThanOrEqualTo Int

data IndexedPredicate = IndexedPredicate Predicate AstC0.Index