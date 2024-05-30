module Environment (createEnvironment, Environment (..), dumpEnvironmentStmts) where

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

createEnvironment :: Storage -> CompileResult Environment
createEnvironment (Storage definitionStorages) = do
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
  Right $ Environment gr start

data Environment = Environment
  { _graph :: !(Gr (AstC2.Ast Int) [IndexedPredicate]),
    _start :: !Node
  }
  deriving (Show, Eq)

dumpEnvironmentStmts :: Environment -> String
dumpEnvironmentStmts = intercalate "\n\n" . map (Display.displayC2 . snd) . labNodes . _graph