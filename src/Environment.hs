module Environment (createEnvironment, Environment (..), dumpEnvironmentStmts) where

import Ast0 qualified
import AstC2 qualified
import AstP0 qualified
import Compile
  ( compile0toRuleDefinition,
    compileDefinition,
    errOnOverlappingPatterns,
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
  rules <- mapM Compile.compile0toRuleDefinition asts
  rules' :: [(([IndexedPredicate], SrcLocked AstP0.Ast), SrcLocked (AstC2.Ast Int))] <-
    mapM compileDefinition rules
  let predicatesP0Pairs = map fst rules'
  errOnOverlappingPatterns predicatesP0Pairs
  let rules'' :: [([IndexedPredicate], [AstC2.Stmt Int])]
      rules'' = map (\((a, _), c) -> (a, uncofree c)) rules'

      start :: Int
      start = 0

      lnodes :: [(Int, AstC2.Ast Int)]
      lnodes = (start, []) : zip [(start + 1) ..] (map snd rules'')

      ledges :: [(Int, Int, [IndexedPredicate])]
      ledges = zipWith (\i p -> (start, i, p)) [(start + 1) ..] (map fst rules'')

      gr :: Gr (AstC2.Ast Int) [IndexedPredicate]
      gr = mkGraph lnodes ledges
  Right $ Environment gr start

dumpEnvironmentStmts :: Environment -> String
dumpEnvironmentStmts = intercalate "\n\n" . map (Display.displayC2 . snd) . labNodes . _graph