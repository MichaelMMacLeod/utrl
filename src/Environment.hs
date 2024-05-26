module Environment (createEnvironment, Environment (..), dumpEnvironmentStmts) where

import Ast0 qualified
import AstC2 qualified
import Compile
  ( compile0ToDefinition,
    compileDefinition,
    errOnOverlappingPatterns,
  )
import CompileTypes
  ( CompiledDefinition (..),
  )
import Data.Graph.Inductive (Graph (labNodes, mkGraph), Node)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (intercalate)
import Display qualified
import Error (CompileResult)
import Predicate (IndexedPredicate)
import Read (SrcLocked)
import Utils (uncofree)

data Environment = Environment
  { _graph :: !(Gr (AstC2.Ast Int) [IndexedPredicate]),
    _start :: !Node
  }
  deriving (Show, Eq)

createEnvironment :: [SrcLocked Ast0.Ast] -> CompileResult Environment
createEnvironment asts = do
  definitions <- mapM compile0ToDefinition asts
  compiledDefinitions <- mapM compileDefinition definitions
  errOnOverlappingPatterns compiledDefinitions
  let predicatesConstructorPairs :: [([IndexedPredicate], [AstC2.Stmt Int])]
      predicatesConstructorPairs =
        map (\d -> (d.predicates, uncofree d.constructor)) compiledDefinitions

      start :: Int
      start = 0

      lnodes :: [(Int, AstC2.Ast Int)]
      lnodes = (start, []) : zip [(start + 1) ..] (map snd predicatesConstructorPairs)

      ledges :: [(Int, Int, [IndexedPredicate])]
      ledges = zipWith (\i p -> (start, i, p)) [(start + 1) ..] (map fst predicatesConstructorPairs)

      gr :: Gr (AstC2.Ast Int) [IndexedPredicate]
      gr = mkGraph lnodes ledges
  Right $ Environment gr start

dumpEnvironmentStmts :: Environment -> String
dumpEnvironmentStmts = intercalate "\n\n" . map (Display.displayC2 . snd) . labNodes . _graph