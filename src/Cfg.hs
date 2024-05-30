module Cfg (mkCfg, Cfg (..), dumpCfgStmts) where

import AstC2 qualified
import Compile (requestConstructorC2, requestPredicates)
import CompileTypes (Storage (..))
import Data.Graph.Inductive (Graph (labNodes, mkGraph), Node)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (intercalate)
import Display qualified
import Error (CompileResult)
import Predicate (IndexedPredicate)
import Utils (uncofree)

mkCfg :: Storage -> CompileResult Cfg
mkCfg (Storage definitionStorages) = do
  constructorDefStoragePairs <- mapM requestConstructorC2 definitionStorages
  let constructorC2s = map fst constructorDefStoragePairs
      definitionStorages' = map snd constructorDefStoragePairs
  predicatesDefStoragePairs <- mapM requestPredicates definitionStorages'
  let predicates = map fst predicatesDefStoragePairs

      start :: Int
      start = 0

      lnodes :: [(Int, AstC2.Ast Int)]
      lnodes = (start, []) : zip [(start + 1) ..] (map uncofree constructorC2s)

      ledges :: [(Int, Int, [IndexedPredicate])]
      ledges = zipWith (\i p -> (start, i, p)) [(start + 1) ..] predicates

      gr :: Gr (AstC2.Ast Int) [IndexedPredicate]
      gr = mkGraph lnodes ledges
  Right $ Cfg gr start

data Cfg = Cfg
  { graph :: !(Gr (AstC2.Ast Int) [IndexedPredicate]),
    start :: !Node
  }
  deriving (Show, Eq)

dumpCfgStmts :: Cfg -> String
dumpCfgStmts = intercalate "\n\n" . map (Display.displayC2 . snd) . labNodes . graph