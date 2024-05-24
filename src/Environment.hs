module Environment (createEnvironment, Environment (..), dumpEnvironmentStmts) where

import qualified AstC2
import Compile
  ( compile0toRuleDefinition,
    compileRule2,
    errOnOverlappingPatterns,
  )
import Data.Graph.Inductive (Graph (labNodes, mkGraph), Node)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Display
import Error (CompileResult)
import Predicate (IndexedPredicate)
import qualified Read

data Environment = Environment
  { _graph :: !(Gr (AstC2.Ast Int) [IndexedPredicate]),
    _start :: !Node
  }
  deriving (Show, Eq)

createEnvironment :: Text -> CompileResult Environment
createEnvironment text = do
  asts <- Read.read "./misc/programs/errors/bad-syntax.txt" text
  rules <- mapM Compile.compile0toRuleDefinition asts
  rules' <- mapM compileRule2 rules
  let predicatesP0Pairs = map fst rules'
  errOnOverlappingPatterns predicatesP0Pairs
  let rules'' = map (\((a, _), c) -> (a, c)) rules'
  let start = 0
  let lnodes = (start, []) : zip [(start + 1) ..] (map snd rules'')
  let ledges = zipWith (\i p -> (start, i, p)) [(start + 1) ..] (map fst rules'')
  let gr = mkGraph lnodes ledges
  Right $ Environment gr start

dumpEnvironmentStmts :: Environment -> String
dumpEnvironmentStmts = intercalate "\n\n" . map (Display.displayC2 . snd) . labNodes . _graph