module Predicate
  ( Predicate (..),
    IndexedPredicate (..),
    applyPredicate,
    applyPredicates,
  )
where

import Ast0 qualified
import AstC0 qualified

data Predicate
  = SymbolEqualTo String
  | LengthEqualTo Int
  | LengthGreaterThanOrEqualTo Int
  deriving (Eq, Show, Ord)

toFunc :: Predicate -> (Ast0.Ast -> Bool)
toFunc (SymbolEqualTo str1) (Ast0.Symbol str2) = str1 == str2
toFunc (LengthEqualTo n) (Ast0.Compound xs) = length xs == n
toFunc (LengthGreaterThanOrEqualTo n) (Ast0.Compound xs) = length xs >= n
toFunc _ _ = False

data IndexedPredicate = IndexedPredicate Predicate AstC0.Index
  deriving (Eq, Show, Ord)

applyPredicate :: IndexedPredicate -> Ast0.Ast -> Bool
applyPredicate (IndexedPredicate p i) ast = all (toFunc p) (getAtC0Index i ast)

applyPredicates :: [IndexedPredicate] -> Ast0.Ast -> Bool
applyPredicates ps x = all (($ x) . applyPredicate) ps

getAtC0Index :: AstC0.Index -> Ast0.Ast -> [Ast0.Ast]
getAtC0Index [] ast = [ast]
getAtC0Index _ (Ast0.Symbol _) = []
getAtC0Index (AstC0.ZeroPlus zp : i) (Ast0.Compound xs) =
  if zp < Prelude.length xs
    then getAtC0Index i (xs !! zp)
    else []
getAtC0Index (AstC0.LenMinus lm : i) (Ast0.Compound xs) =
  let zp = Prelude.length xs - lm
   in if zp > 0
        then getAtC0Index i (xs !! zp)
        else []
getAtC0Index (AstC0.Between zp lm : i) (Ast0.Compound xs) =
  let zpEnd = Prelude.length xs - lm
   in if zp < Prelude.length xs && zpEnd > 0
        then concatMap (getAtC0Index i) (drop zp (take zpEnd xs))
        else []
