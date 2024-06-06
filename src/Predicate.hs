module Predicate
  ( Predicate (..),
    IndexedPredicate (..),
    applyPredicate,
    applyPredicates,
    toFuncForOverlappingPatternAnalysis,
    applyPredicateForOverlappingPatternAnalysis,
    applyPredicatesForOverlappingPatternAnalysis,
  )
where

import Ast0 qualified
import AstC0 qualified
import Data.Text (Text)
import Utils (isDollarSignVar)

data Predicate
  = SymbolEqualTo Text
  | LengthEqualTo Int
  | LengthGreaterThanOrEqualTo Int
  deriving (Eq, Show, Ord)

toFunc :: Predicate -> (Ast0.Ast -> Bool)
toFunc (SymbolEqualTo str1) (Ast0.Symbol str2) = str1 == str2
toFunc (LengthEqualTo n) (Ast0.Compound xs) = length xs == n
toFunc (LengthGreaterThanOrEqualTo n) (Ast0.Compound xs) = length xs >= n
toFunc _ _ = False

-- | like 'toFunc', with the only difference being that any symbol starting
-- with a '$' is considered equal to any other symbol.
--
-- When checking to determine if two patterns could possibly match the same
-- term, as in how both the following could match '(0 0)':
--
-- >  (def ($n 0) _)
-- >
-- >  (def (0 $m) _)
--
-- predicates are generated for both patterns, and then applied to each other's
-- original AST. This means that we would apply the test 'p[1] == "0"' to the
-- second pattern, and 'p[0] == "0"' to the first pattern. But, because, in the
-- original AST of the first pattern, 'p[0]' is '"$n"' and not '"0"', we can't
-- simpy use 'toFunc', as that would fail to match, since '"0" != "$n"'. This
-- function makes sure that if the string being compared starts with a dollar-sign,
-- then it always matches.
toFuncForOverlappingPatternAnalysis :: Predicate -> (Ast0.Ast -> Bool)
toFuncForOverlappingPatternAnalysis predicate symbol = case (predicate, symbol) of
  (SymbolEqualTo s1, Ast0.Symbol s2) -> isDollarSignVar s2 || s1 == s2
  (LengthEqualTo n, Ast0.Compound xs) -> length xs == n
  (LengthGreaterThanOrEqualTo n, Ast0.Compound xs) -> length xs >= n
  _ -> False

data IndexedPredicate = IndexedPredicate Predicate AstC0.Index
  deriving (Eq, Show, Ord)

applyPredicate :: IndexedPredicate -> Ast0.Ast -> Bool
applyPredicate (IndexedPredicate p i) ast = all (toFunc p) (getAtC0Index i ast)

applyPredicateForOverlappingPatternAnalysis :: IndexedPredicate -> Ast0.Ast -> Bool
applyPredicateForOverlappingPatternAnalysis (IndexedPredicate p i) ast =
  all (toFuncForOverlappingPatternAnalysis p) (getAtC0Index i ast)

applyPredicates :: [IndexedPredicate] -> Ast0.Ast -> Bool
applyPredicates ps x = all (($ x) . applyPredicate) ps

applyPredicatesForOverlappingPatternAnalysis :: [IndexedPredicate] -> Ast0.Ast -> Bool
applyPredicatesForOverlappingPatternAnalysis ps x =
  all (($ x) . applyPredicateForOverlappingPatternAnalysis) ps

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
